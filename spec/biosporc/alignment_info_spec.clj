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
  {:ref "seq2" :read "read1" :start 199 :end 248 :len 50})
(def last-read 
  {:ref "seq2" :read "read14" :start 1201 :end 1250 :len 50})

(describe "make-sam-reader"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (it "should have an index"
    (should (.hasIndex @sam-reader)))
  (it "returns a sam file reader of the given file"
    (should= (format "SAMFileHeader{%s}" correct-header)
             (str (.getFileHeader @sam-reader)))))

(describe "get-read-info"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (it "gives info about all 14 reads in the test file"
    (should= 14 (count @read-info)))
  (it "returns info about the reads (first matches)"
    (should= first-read (first @read-info)))
  (it "returns info about the reads (last matches)"
    (should= last-read (last @read-info))))

(describe "get-reads"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (context "when querying contained reads"
    (it "gives the reads that are contained in interval but not overlapping"
      (should= (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}]) 
               (get-reads "seq2" 200 280 @sam-reader query-contained-reads))))
  (context "when querying overlapping reads"
    (it "gives the reads that are both contained and overlapping the interval"
      (should= (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                     {:read "read2" :ref "seq2" :start 225 :end 274 :len 50}
                     {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])
               (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads)))))

(describe "bin-reads"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with contained-reads 
        (get-reads "seq2" 200 280 @sam-reader query-contained-reads))
  (with overlapping-reads 
        (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads))
  (it "returns a map with the islanders and bridgers for a given region"
    (should= {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
              :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                              {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
             (bin-reads @contained-reads @overlapping-reads))))

(describe "single-orf-alignment-info"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-map (hash-map :orf "orf1" :ref "seq2" :start 200 :end 280 :len 81))
  (it "takes an orf map and gives alignment info for it"
    (should= 
     {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
      :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                      {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
     (single-orf-alignment-info @orf-map @sam-reader))))

(describe "alignment-info"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-map (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 :len 81}
                        {:orf "orf2" :ref "seq2" :start 200 :end 280 :len 81}))
  (it "calls single-orf-alignment info for each orf-map in given collection"
    (should= (hash-map :orf1
                       {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
                        :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                                        {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
                       :orf2
                       {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
                        :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                                        {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])})
             (alignment-info @orf-map @sam-reader))))

(describe "alignment-info-for-random-orf-maps"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-map (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 :len 81}
                        {:orf "orf2" :ref "seq2" :start 200 :end 280 :len 81}))
  (it "calls single-orf-alignment info for each orf-map in given collection but throw them in a vec"
    (should= (seq (vector {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
                           :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                                           {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
                          {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
                           :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                                           {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}))
             (alignment-info-for-random-orf-maps @orf-map @sam-reader))))
