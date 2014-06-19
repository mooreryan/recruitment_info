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
                     (assoc avg-covs ref (+ (:inferred-insert-size read)
                                            (ref avg-covs))))
              (and (:inferred-insert-size read)
                   (:first read))
              (recur (rest read-info-maps)
                     (assoc avg-covs ref (:inferred-insert-size read)))
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

;; TODO consider laziness by using filters and lazy-seq around the
;; iterator-seq instead of set differences

(defn query-contained-reads [seq start end sam-reader]
  (.queryContained sam-reader seq start end))

(defn query-overlapping-reads [seq start end sam-reader]
  (.queryOverlapping sam-reader seq start end))

;; (defn get-reads 
;;   "Gets reads that are either contained or overlapping the given
;;   interval depending on whehter query-contained-reads or
;;   query-overlapping-reads is passed. Closes the iterator."  
;;   [seq start end sam-reader query-fn]
;;   (let [iter (query-fn seq start end sam-reader)
;;         reads (iterator-seq iter)
;;         read-set (set (map get-record-info reads))]
;;     (.close iter)
;;     read-set))

;; (defn bin-reads 
;;   "Partitions the contained and overlapping reads into islanders and
;;   bridgers."
;;   [contained-reads overlapping-reads]
;;   (hash-map :islanders contained-reads
;;             :bridgers (clojure.set/difference overlapping-reads contained-reads)))

;; (defn single-orf-alignment-info [orf-map sam-reader]
;;   (let [get-reads-par (partial get-reads 
;;                                (:ref orf-map)
;;                                (:start orf-map)
;;                                (:end orf-map)
;;                                sam-reader)
;;         contained-reads (get-reads-par query-contained-reads)
;;         overlapping-reads (get-reads-par query-overlapping-reads)]
;;     (bin-reads contained-reads overlapping-reads)))

;; (defn alignment-info [orf-maps sam-reader]
;;   (into (hash-map) 
;;         (map (fn [orf-map]
;;                (hash-map (keyword (:orf orf-map)) 
;;                          (single-orf-alignment-info orf-map sam-reader)))
;;              orf-maps)))

;; (defn alignment-info-for-random-orf-maps [orf-maps sam-reader]
;;   (map (fn [orf-map]
;;          (single-orf-alignment-info orf-map sam-reader))
;;        orf-maps))
