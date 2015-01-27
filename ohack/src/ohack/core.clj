(ns ohack.core
  (:use [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.sampled-piano])
  (:require [overtone.inst.synth :as synth]))

;; run M-x cider-jack-in to get started
;;
;; once started, move your cursor to the (ns ...) block above and hit
;; C-M-x to evaluate it and boot the supercollider server.  if your
;; sampled-piano samples are not cached, overtime will attempt to
;; download them and cache them now. once you see the prompt update with
;; the Overtone logo, you are good to go.
