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
   :mate-ref-name nil
   :inferred-insert-size nil})
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
   :mate-ref-name nil
   :inferred-insert-size nil})

(describe "make-sam-reader"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (it "should have an index"
    (should (.hasIndex @sam-reader)))
  (it "returns a sam file reader of the given file"
    (should= (format "SAMFileHeader{%s}" correct-header)
             (str (.getFileHeader @sam-reader)))))

(describe "get-reference-lengths"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (it "gets lengths for all the references"
    (should= {:seq1 5000 :seq2 5000}
             (get-reference-lengths @sam-reader))))

(describe "get-all-align-info"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (it "gives info about all 14 reads in the test file"
    (pending)
    (should= 14 (count @read-info)))
  (it "returns info about the reads (first matches)"
    (pending)
    (should= first-read (first @read-info)))
  (it "returns info about the reads (last matches)"
    (pending)
    (should= last-read (last @read-info)))
  (context "with a proper pair"
    (it "always returns a positive inferred insert size"
      (pending "it does this, but I need a test"))))

(def reads (seq [{:ref "seq2" :read "read1" :start 100 :end 200 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq2" :read "read2" :start 150 :end 250 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq1" :read "read3" :start 100 :end 200 :len 101
                  :mapped false :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}
                 {:ref "seq1" :read "read4" :start 100 :end 200 :len 101
                  :mapped true :read-paired false :proper-pair nil
                  :first nil :second nil :mate-ref-name nil}]))


(describe "count-mapped-reads-per-ref"
  (with outdir (str "/Users/ryanmoore/projects/wommack/recruitment_info/"
                    "test_files/test_output"))
  (with id "mapped_reads")
  (it "counts the number of reads mapped to each reference"
    (should= {:seq1 1 :seq2 2}
             (count-mapped-reads-per-ref reads {:seq1 500 :seq2 600} @outdir)))
  (it "outputs a coverage graph for each reference"
    (should (and (.exists (clojure.java.io/file (format "%s/seq1_cov_%s.pdf" 
                                                        @outdir @id)))
                 (.exists (clojure.java.io/file (format "%s/seq2_cov_%s.pdf" 
                                                        @outdir @id)))))))

;; this is how i think things work with the various flags
(def reads2 (seq [{:ref "seq2" :read "read1" :mapped true
                   :read-paired true :proper-pair true :first true
                   :second false :mate-mapped true
                   :inferred-insert-size 500
                   :start 100 :end 599 ;; TODO to fix failing test add mate-alignment-start to each of these
                   }
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
                   :second false :mate-mapped true
                   :inferred-insert-size 600
                   :start 400 :end 999}
                  {:ref "seq2" :read "read6" :mapped true
                   :read-paired true :proper-pair true :first false
                   :second true :mate-mapped true}]))

(describe "count-proper-fragments-per-ref"
  (with outdir (str "/Users/ryanmoore/projects/wommack/recruitment_info/"
                    "test_files/test_output"))
  (with id "mapped_proper_frags")
  (it "counts the number of proper fragments mapped to each ref"
    (pending)
    (should= {:seq2 2}
             (count-proper-fragments-per-ref reads2 {:seq2 2000} @outdir)))
  (it "outputs a coverage graph for each reference"
    (pending)
    (should (.exists 
             (clojure.java.io/file 
              (format "%s/seq2_cov_mapped_proper_fragments.pdf" 
                      @outdir)))))
  (it "wont extend reads beyond the reference length"
    (pending))
  (it "extends the read farthest to the left, whether or not it is first or second"
    (pending)))

(describe "avg-mapped-read-cov"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (with ref-lengths {:seq2 5000})

  (it "returns average coverage for a reference"
    (should= {:seq2 7/50}
            (avg-mapped-read-cov @read-info @ref-lengths))))

(def reads (seq [{:ref "seq2" :read "read1" :start 100 :end 199 
                  :len 100 :mapped true :read-paired true
                  :proper-pair true :first true :second false
                  :mate-mapped true :mate-ref-name "seq2"
                  :inferred-insert-size 500}
                 {:ref "seq2" :read "read2" :start 499 :end 598 
                  :len 100 :mapped true :read-paired true
                  :proper-pair true :first false :second true
                  :mate-mapped true :mate-ref-name "seq2"
                  :inferred-insert-size 500}
                 {:ref "seq2" :read "read3" :start 800 :end 899 
                  :len 100 :mapped true :read-paired true
                  :proper-pair false :first true :second false
                  :mate-mapped false :mate-ref-name "seq2"
                  :inferred-insert-size nil}
                 {:ref "seq2" :read "read4" :start 1200 :end 1299 
                  :len 100 :mapped true :read-paired true
                  :proper-pair false :first false :second true
                  :mate-mapped false :mate-ref-name "seq2"
                  :inferred-insert-size nil}]))

(describe "avg-proper-fragment-cov"
  (with ref-lengths {:seq2 5000})
  (it "returns mean cov for references based on proper pairs"
    (should= {:seq2 1/10}
             (avg-proper-fragment-cov reads @ref-lengths))))

(describe "print-cov-info"
  (with outdir (str "/Users/ryanmoore/projects/wommack/recruitment_info/"
                    "test_files/test_output"))
  (with ref-lengths {:seq2 5000})
  (with reads (seq [{:ref "seq2" :read "read1" :start 100 :end 199 
                     :len 100 :mapped true :read-paired true
                     :proper-pair true :first true :second false
                     :mate-mapped true :mate-ref-name "seq2"
                     :inferred-insert-size 1000}
                    {:ref "seq2" :read "read2" :start 1000 :end 1099 
                     :len 100 :mapped true :read-paired true
                     :proper-pair true :first false :second true
                     :mate-mapped true :mate-ref-name "seq2"
                     :inferred-insert-size 1000}
                    {:ref "seq2" :read "read3" :start 800 :end 899 
                     :len 100 :mapped true :read-paired true
                     :proper-pair false :first true :second false
                     :mate-mapped false :mate-ref-name "seq2"
                     :inferred-insert-size nil}
                    {:ref "seq2" :read "read4" :start 1200 :end 1299 
                     :len 100 :mapped true :read-paired true
                     :proper-pair false :first false :second true
                     :mate-mapped false :mate-ref-name "seq2"
                     :inferred-insert-size nil}
                    {:ref "seq2" :read "read4" :start 1200 :end 1299 
                     :len 100 :mapped true :read-paired true
                     :proper-pair false :first true :second false
                     :mate-mapped false :mate-ref-name "seq2"
                     :inferred-insert-size nil}
                    {:ref "seq2" :read "read4" :start 1200 :end 1299 
                     :len 100 :mapped false :read-paired true
                     :proper-pair false :first false :second true
                     :mate-mapped false :mate-ref-name "seq2"
                     :inferred-insert-size nil}]))
  (it "prints all the coverage metrics"
    (pending)
    (should= (seq ["seq2\t5000\t5\t0.1\t1\t0.2"])
             (print-cov-info @reads @ref-lengths @outdir))))

#_(describe "read-maps-to-map"
  (it "puts the coll of read maps into a map"
    (should= {:read1 {:read "read1" :start 100 :end 200} :read-2 
              {:read "read 2" :start 1000 :end 2000}}
             (read-maps-to-map [{:read "read1" :start 100 :end 200} 
                                {:read "read 2" :start 1000 :end 2000}]))))

(describe "is-this-read-lower?"
  (with read-map-that-is-lower {:start 1 :mate-alignment-start 100})
  (with read-map-that-is-not-lower {:start 100 :mate-alignment-start 1})
  (it "returns the start posn if this read is lower"
    (should= 1 (is-this-read-lower? @read-map-that-is-lower)))
  (it "returns nil if the start posn of its mate is lower"
    (should-not (is-this-read-lower? @read-map-that-is-not-lower))))

(describe "extend-read"
  (with read-map {:start 10 :end 100 :mate-alignment-start 150})
  (it "returns a map with the end adjusted to one less that the mate-alignment-start"
    (should= {:start 10 :end 149 :mate-alignment-start 150}
             (extend-read @read-map))))
