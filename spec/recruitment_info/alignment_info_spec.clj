(ns recruitment_info.alignment-info-spec
  (:require [speclj.core :refer :all]
            [recruitment_info.alignment-info :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/recruitment_info")
(def sorted-bam
  (str base "/test_files/unpaired.sorted.bam"))
(def bam-index
  (str base "/test_files/unpaired.sorted.bam.bai"))
(def correct-header "VN=1.5 SO:coordinate")
(def first-read 
  {:ref "seq2" 
   :read "read1" 
   :start 199 
   :end 248 
   :len 50
   :mapped true
   :mate-mapped nil
   :read-paired false
   :proper-pair nil
   :first nil
   :second nil
   :mate-ref-name nil})
(def last-read 
  {:ref "seq2" 
   :read "read14" 
   :start 1201 
   :end 1250 
   :len 50
   :mapped true
   :mate-mapped nil
   :read-paired false
   :proper-pair nil
   :first nil
   :second nil
   :mate-ref-name nil})

(describe "make-sam-reader"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (it "should have an index"
    (should (.hasIndex @sam-reader)))
  (it "returns a sam file reader of the given file"
    (should= (format "SAMFileHeader{%s}" correct-header)
             (str (.getFileHeader @sam-reader)))))

(describe "get-all-align-info"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (it "gives info about all 14 reads in the test file"
    (should= 14 (count @read-info)))
  (it "returns info about the reads (first matches)"
    (should= first-read (first @read-info)))
  (it "returns info about the reads (last matches)"
    (should= last-read (last @read-info))))

(def reads (seq [{:ref "seq2" :read "read1" :start 100 :end 200 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq2" :read "read2" :start 100 :end 200 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq1" :read "read3" :start 100 :end 200 :len 101
                  :mapped false :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq1" :read "read4" :start 100 :end 200 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}]))
(describe "count-mapped-reads-per-ref"
  (it "counts the number of reads mapped to each reference"
    (should= {:seq1 1 :seq2 2}
             (count-mapped-reads-per-ref reads))))

;; this is how i think things work with the various flags
(def reads2 (seq [{:ref "seq2" :read "read1" :mapped true
                   :read-paired true :proper-pair true :first true
                   :second false :mate-mapped true}
                  {:ref "seq2" :read "read2" :mapped true
                   :read-paired true :proper-pair true :first false
                   :second true :mate-mapped true}
                  {:ref "seq2" :read "read3" :mapped true
                   :read-paired true :proper-pair false :first true
                   :second false :mate-mapped true}
                  {:ref "seq2" :read "read4" :mapped false
                   :read-paired true :proper-pair false :first false
                   :second true :mate-mapped true}
                  {:ref "seq2" :read "read5" :mapped true
                   :read-paired true :proper-pair true :first true
                   :second false :mate-mapped true}
                  {:ref "seq2" :read "read6" :mapped true
                   :read-paired true :proper-pair true :first false
                   :second true :mate-mapped true}]))

(describe "count-proper-fragments-per-ref"
  (it "counts the number of proper fragments mapped to each ref"
    (should= {:seq2 2}
             (count-proper-fragments-per-ref reads2))))

(def count-info {:seq1 1000, :seq2 1000})

#_(describe "avg-cov"
  (it "returns average coverage for a reference"
    ))

;; (describe "get-reads"
;;   (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
;;   (context "when querying contained reads"
;;     (it "gives the reads that are contained in interval but not overlapping"
;;       (should= (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}]) 
;;                (get-reads "seq2" 200 280 @sam-reader query-contained-reads))))
;;   (context "when querying overlapping reads"
;;     (it "gives the reads that are both contained and overlapping the interval"
;;       (should= (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                      {:read "read2" :ref "seq2" :start 225 :end 274 :len 50}
;;                      {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])
;;                (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads)))))

;; (describe "bin-reads"
;;   (with sam-reader 
;;         (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
;;   (with contained-reads 
;;         (get-reads "seq2" 200 280 @sam-reader query-contained-reads))
;;   (with overlapping-reads 
;;         (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads))
;;   (it "returns a map with the islanders and bridgers for a given region"
;;     (should= {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;               :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                               {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
;;              (bin-reads @contained-reads @overlapping-reads))))

;; (describe "single-orf-alignment-info"
;;   (with sam-reader 
;;         (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
;;   (with orf-map (hash-map :orf "orf1" :ref "seq2" :start 200 :end 280 :len 81))
;;   (it "takes an orf map and gives alignment info for it"
;;     (should= 
;;      {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;       :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                       {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
;;      (single-orf-alignment-info @orf-map @sam-reader))))

;; (describe "alignment-info"
;;   (with sam-reader 
;;         (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
;;   (with orf-map (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 :len 81}
;;                         {:orf "orf2" :ref "seq2" :start 200 :end 280 :len 81}))
;;   (it "calls single-orf-alignment info for each orf-map in given collection"
;;     (should= (hash-map :orf1
;;                        {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;                         :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                                         {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
;;                        :orf2
;;                        {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;                         :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                                         {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])})
;;              (alignment-info @orf-map @sam-reader))))

;; (describe "alignment-info-for-random-orf-maps"
;;   (with sam-reader 
;;         (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
;;   (with orf-map (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 :len 81}
;;                         {:orf "orf2" :ref "seq2" :start 200 :end 280 :len 81}))
;;   (it "calls single-orf-alignment info for each orf-map in given collection but throw them in a vec"
;;     (should= (seq (vector {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;                            :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                                            {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
;;                           {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
;;                            :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
;;                                            {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}))
;;              (alignment-info-for-random-orf-maps @orf-map @sam-reader))))
