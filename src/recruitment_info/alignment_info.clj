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
  (:require [clojure.java.io :as io])
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
  "This works on bam records from the iterator opened on the sam-reader."
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
              :mate-ref-name (if paired 
                               (.getMateReferenceName bam-record))
              :inferred-insert-size ; might be zero
              ;; should keep things from being negative or zero
              (if proper-pair
                (.getInferredInsertSize bam-record)))))

(defn get-all-align-info
  "Returns a seq of maps containing info for all sequences."
  [sam-reader]
  (let [iter (lazy-seq (iterator-seq (.iterator sam-reader)))]
    (map get-record-info iter)))

(defn inc-counts [reference counts]
  (assoc counts reference (inc (reference counts))))

(defn count-mapped-reads-per-ref
  "Count the number of mapped reads per reference given a seq of
  read-info maps (ie the output from get-all-align-info)"
  [read-info-maps]
  (loop [read-info read-info-maps
         counts {}]
    (if-not (empty? (first read-info))
      (let [read (:read (first read-info)) 
            reference (keyword (:ref (first read-info)))]
        (cond (and (contains? counts reference) 
                   (:mapped (first read-info)))
              (recur (rest read-info) 
                     (inc-counts reference counts))
              (contains? counts reference)
              (recur (rest read-info) 
                     counts)
              (:mapped (first read-info))
              (recur (rest read-info) 
                     (assoc counts reference 1))
              :else
              (recur (rest read-info) counts)))
      counts)))

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

(defn count-proper-fragments-per-ref
  "This makes some assumptions about how the mate flags work. It
  assumes that if the proper-pair flag is true, then both the first
  and the second mates will both me mapping to the SAME reference and
  thus both be in the read-info-maps seq. So it increments only if the
  read is the first in the pair. Also, it assumes that a proper-pair
  flag means both pairs are in fact mapped. TODO: This should be
  double checked with recruitment software docs."  
  [read-info-maps]
  (loop [read-info read-info-maps
         counts {}]
    (if-not (empty? (first read-info))
      (let [read (first read-info)
            ref (keyword (:ref read))]
        (cond (and (contains? counts ref)
                   (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:first read))
              (recur (rest read-info)
                     (inc-counts ref counts))
              (and (:mapped read)
                   (:mate-mapped read)
                   (:proper-pair read)
                   (:first read))
              (recur (rest read-info)
                     (assoc counts ref 1))
              :else
              (recur (rest read-info) counts)))
      counts)))

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
                            (+ (Math/abs (:inferred-insert-size read))
                               (ref avg-covs))))
              (and (:inferred-insert-size read)
                   (:first read))
              (recur (rest read-info-maps)
                     (assoc avg-covs ref 
                            (Math/abs (:inferred-insert-size read))))
              :else
              (recur (rest read-info-maps)
                     avg-covs)))
      (avg-cov avg-covs ref-lengths))))

(defn to-num [r]
  (cond 
   (= clojure.lang.Ratio (class r)) (double r)
   (= java.lang.Long (class r)) r
   (nil? r) 0))

(defn print-cov-info [read-info-maps ref-lengths]
  (let [references (keys ref-lengths)
        all-reads (count-mapped-reads-per-ref read-info-maps)
        all-read-cov (avg-mapped-read-cov read-info-maps ref-lengths)
        proper-frags (count-proper-fragments-per-ref read-info-maps)
        proper-frag-cov (avg-proper-fragment-cov read-info-maps 
                                                 ref-lengths)]
    (for [ref references]
      (clojure.string/join "\t" 
                           (vector (name ref)
                                   (to-num (ref all-reads))
                                   (to-num (ref all-read-cov))
                                   (to-num (ref proper-frags))
                                   (to-num (ref proper-frag-cov)))))))

(defn alignment-info 
  "Worker for this namespace."
  [sorted-bam bam-index]
  (let [sam-reader (make-sam-reader (make-sam-reader-factory)
                                    sorted-bam bam-index)
        read-info-maps (get-all-align-info sam-reader)
        ref-lengths (get-reference-lengths sam-reader)]
    (print-cov-info read-info-maps ref-lengths)))
