(ns recruitment_info.core-spec
  (:require [speclj.core :refer :all]
            [recruitment_info.core :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/recruitment_info")
(def sorted-bam
  (str base "/test_files/unpaired.sorted.bam"))
(def bam-index
  (str base "/test_files/unpaired.sorted.bam.bai"))



(describe "-main"
  (context "with proper arguments"
    (it "gets alignment info from a sorted bam and index"
      (should= '("seq2\t14\t0.14\t0\t0" "seq1\t0\t0\t0\t0")
               (-main "-b" sorted-bam "-i" bam-index)))))
