;; Copyright 2014 Ryan Moore

;; This file is part of recruitment_info.

;; recruitment_info is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; recruitment_info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with recruitment_info.  If not, see
;; <http://www.gnu.org/licenses/>.

(ns recruitment_info.alignment-info
  (:require [clojure.java.io :as io]
            [recruitment_info.plots :as plots])
  (:import (htsjdk.samtools SamReaderFactory
                            SamInputResource
                            ValidationStringency)))

(defn make-sam-reader-factory []
  (.validationStringency (SamReaderFactory/makeDefault)
                         (ValidationStringency/valueOf "SILENT")))

(defn make-sam-reader 
  "TODO: Doesn't handle file exceptions"
  [sr-fac sorted-bam bam-index]
  (.open sr-fac
         (.index (SamInputResource/of (io/file sorted-bam))
                 (io/file bam-index))))

(defn get-reference-lengths 
  "From the sam-reader, return a map with keys and values being
  references and lengths respectively."
  [sam-reader]
  (let [sam-file-header (.getFileHeader sam-reader)
        sam-sequence-dictionary (.getSequenceDictionary sam-file-header)
        sequences (.getSequences sam-sequence-dictionary)]
    (zipmap
     (map #(keyword (.getSequenceName %)) sequences)
     (map #(.getSequenceLength %) sequences))))

(defn get-length [start end]
  (inc (- end start)))

(defn get-record-info 
  "This works on bam records from the iterator opened on the
  sam-reader. Not called on it's own, but in get-all-align-info."
  [bam-record]
  (let [start (.getAlignmentStart bam-record)
        end (.getAlignmentEnd bam-record)
        paired (.getReadPairedFlag bam-record)
        proper-pair (if paired (.getProperPairFlag bam-record))]
    (hash-map :ref (.getReferenceName bam-record)
              :read (.getReadName bam-record)
              :start start
              :end end
              :len (get-length start end)
              :mapped (not (.getReadUnmappedFlag bam-record))
              :read-paired paired
              :proper-pair proper-pair
              :first (if paired (.getFirstOfPairFlag bam-record))
              :second (if paired (.getSecondOfPairFlag bam-record))
              :mate-mapped (if paired 
                             (not (.getMateUnmappedFlag bam-record)))
              :mate-alignment-start (if paired
                                      (.getMateAlignmentStart bam-record))
              :mate-ref-name (if paired 
                               (.getMateReferenceName bam-record))
              :inferred-insert-size ; might be zero
              ;; should keep things from being negative or zero
              (if proper-pair
                (Math/abs (.getInferredInsertSize bam-record))))))

(defn get-all-align-info
  "Returns a seq of maps containing info for all sequences."
  [sam-reader]
  (let [iter (lazy-seq (iterator-seq (.iterator sam-reader)))]
    (map get-record-info iter)))

#_(defn read-maps-to-map
    "Takes the read maps from get-all-align info and puts them into a
  map with key being the read name and value being the whole read
  map. Will be used to quickly lookup a mate's pair for extending the
  reads for proper pairs."
    [read-maps]
    (loop [read-maps read-maps new-map {}]
      (if (seq (first read-maps))
        (if (contains? new-map (:read (first read-maps)))
          (do (println (format "Duplicate read: %s for reference: %s"
                               (:read (first read-maps)) (:ref (first read-maps))))
              (System/exit 4))
          (recur (rest read-maps) (assoc new-map 
                                    (keyword (clojure.string/replace 
                                              (:read (first read-maps))
                                              #" " "-")) 
                                    (first read-maps))))
        new-map)))

(defn inc-counts [reference counts]
  (assoc counts reference (inc (reference counts))))

(defn count-mapped-reads-per-ref
  "Count the number of mapped reads per reference given a seq of
  read-info maps (ie the output from get-all-align-info)"
  [read-info-maps ref-lengths outdir]
  (loop [read-info read-info-maps
         counts {}
         info {}]
    (if-not (empty? (first read-info))
      (let [read (first read-info) 
            reference (keyword (:ref read))]
        (cond (and (contains? counts reference) 
                   (:mapped read))
              (recur (rest read-info) 
                     (inc-counts reference counts)
                     (assoc info reference 
                            (conj (reference info) read)))
              ;; ref in the map, read not mapped
              (contains? counts reference) 
              (recur (rest read-info) 
                     counts 
                     info)
              (:mapped read)
              (recur (rest read-info) 
                     (assoc counts reference 1)
                     (assoc info reference [read]))
              :else
              (recur (rest read-info) counts info)))
      (do
        (doall 
         (plots/plot-cov-for-info-map info 
                                      ref-lengths 
                                      outdir
                                      "mapped_reads"))
        counts))))

(defn is-this-read-lower?
  "Checks to see if the given read or it's mate starts farther to the
  left. Returns the start position if this read is farther left, nil
  if the mate is farther left."
  [read-map]
  (if (< (- (:start read-map) (:mate-alignment-start read-map)) 0)
         (:start read-map)))

(defn extend-read
  "Extends the read to one less than the mate-alignment-start. Input
  is assumed to be correct."
  [read-map]
  (assoc read-map :end (dec (:mate-alignment-start read-map))))

(defn extend-read-if-needed
  "Combines the two previous functions into one."
  [read-map]
  (if (is-this-read-lower? read-map)
    (extend-read read-map)
    read-map))

(defn count-proper-fragments-per-ref
  "This makes some assumptions about how the mate flags work. It
  assumes that if the proper-pair flag is true, then both the first
  and the second mates will both me mapping to the SAME reference and
  thus both be in the read-info-maps seq. So it increments only if the
  read is the first in the pair. Also, it assumes that a proper-pair
  flag means both pairs are in fact mapped. TODO: This should be
  double checked with recruitment software docs."  
  [read-info-maps ref-lengths outdir]
  (loop [read-info read-info-maps
         counts {}
         info {}]
    (if (empty? (first read-info))
      (do
        (doall
         (plots/plot-cov-for-info-map info 
                                      ref-lengths 
                                      outdir
                                      "mapped_proper_fragments"))
        counts)
      (let [read (first read-info)
            reference (keyword (:ref read))]
        (cond (and (contains? counts reference)
                   (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:first read))
              (recur (rest read-info)
                     (inc-counts reference counts)
                     ;; TODO: does counts having reference mean info has it to?
                     (assoc info reference
                            (conj (reference info) 
                                  (extend-read-if-needed read))))
              (and (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:first read))
              (recur (rest read-info)
                     (assoc counts reference 1)
                     (assoc info reference [(extend-read-if-needed read)]))
              (and (contains? counts reference) 
                   (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:second read))
              (recur (rest read-info)
                     counts
                     (assoc info reference
                            (conj (reference info) 
                                  (extend-read-if-needed read))))
              (and (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:second read))
              (recur (rest read-info)
                     counts
                     (assoc info reference [(extend-read-if-needed read)]))
              :else
              (recur (rest read-info) counts info))))))

(defn avg-cov [avg-covs count-info-map]
  (into {} 
        (for [entry avg-covs] 
          (vector (first entry) 
                  (/ (last entry)
                     ((first entry)
                      count-info-map))))))

(defn avg-mapped-read-cov 
  "Does a naive calculation of mean coverage. Takes literally every
  read that is mapped, regardless of paired info and sums the total
  bases covered divided by the length of the reference for each
  reference."  
  [read-info-maps ref-lengths]
  (loop [read-info-maps read-info-maps
         avg-covs {}]
    (if-not (empty? (first read-info-maps))
      (let [read (first read-info-maps)
            ref (keyword (:ref read))]
        (cond (and (contains? avg-covs ref)
                   (:mapped read)) 
              (recur (rest read-info-maps)
                     (assoc avg-covs ref (+ (:len read)
                                            (ref avg-covs))))
              (:mapped read)
              (recur (rest read-info-maps)
                     (assoc avg-covs ref (:len read)))
              :else
              (recur (rest read-info-maps)
                     avg-covs)))
      (avg-cov avg-covs ref-lengths))))



(defn avg-proper-fragment-cov [read-info-maps ref-lengths]
  (loop [read-info-maps read-info-maps
         avg-covs {}]
    (if-not (empty? (first read-info-maps))
      (let [read (first read-info-maps)
            ref (keyword (:ref read))]
        (cond (and (contains? avg-covs ref)
                   (:inferred-insert-size read)
                   (:first read))
              (recur (rest read-info-maps)
                     (assoc avg-covs ref 
                            (+ (:inferred-insert-size read)
                               (ref avg-covs))))
              (and (:inferred-insert-size read)
                   (:first read))
              (recur (rest read-info-maps)
                     (assoc avg-covs ref 
                            (:inferred-insert-size read)))
              :else
              (recur (rest read-info-maps)
                     avg-covs)))
      (avg-cov avg-covs ref-lengths))))

(defn to-num [r]
  (cond 
   (= clojure.lang.Ratio (class r)) (double r)
   (= java.lang.Long (class r)) r
   (= java.lang.Integer (class r)) r
   (nil? r) 0))

(defn print-cov-info [read-info-maps ref-lengths outdir]
  (let [references (keys ref-lengths)
        all-reads (count-mapped-reads-per-ref read-info-maps ref-lengths outdir)
        all-read-cov (avg-mapped-read-cov read-info-maps ref-lengths)
        proper-frags (count-proper-fragments-per-ref read-info-maps ref-lengths
                                                     outdir)
        proper-frag-cov (avg-proper-fragment-cov read-info-maps 
                                                 ref-lengths)]
    (for [refn references]
      (clojure.string/join "\t" 
                           (vector (name refn)
                                   (to-num (refn ref-lengths))
                                   (to-num (refn all-reads))
                                   (to-num (refn all-read-cov))
                                   (to-num (refn proper-frags))
                                   (to-num (refn proper-frag-cov)))))))

(defn alignment-info 
  "Worker for this namespace."
  [sorted-bam bam-index outdir]
  (let [sam-reader (make-sam-reader (make-sam-reader-factory)
                                    sorted-bam bam-index)
        read-info-maps (get-all-align-info sam-reader)
        ref-lengths (get-reference-lengths sam-reader)]
    (print-cov-info read-info-maps ref-lengths outdir)))
