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


;; use odoc() to lookup function documentation in overtone outside of
;; this you should probably use doc() for the same purpose

;; get documentation for prn
(odoc prn)

;; get documentation for synth/ping
(odoc synth/ping)

;; get documentation for odoc
(odoc odoc)


;; so `synth` contains a bunch of instrument definitions that you can
;; use already. you can try them by calling e.g.
(synth/ping)

;; some of them will fade away after a while.
(synth/rise-fall-pad)

;; some of them don't
(synth/vintage-bass)

;; in which case, stop(), i.e. overtone.live/stop(), switches off all sounds
(stop)

;; tip: C-M-x evals the expression (to the outermost surrounding
;; expression!) that your cursor is /inside/, or /before/. To evaluate
;; the expression that your cursor is /immdiately after/, use C-x-e

;; other synths to try
(comment
  (synth/daf-bass)
  (synth/buzz)
  (synth/cs80lead)
  (synth/ticker)
  (synth/pad)
  (synth/bass)
  (synth/daf-bass)
  (synth/grunge-bass)
  (synth/ks1)

  (stop)
  )
