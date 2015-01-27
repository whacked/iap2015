(ns ohack.core
  (:use [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.sampled-piano]
        [overtone.synth.stringed])
  (:require [overtone.inst.synth :as synth]
            [clojure.string :as s]))

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

;; now let's play a little melody
(let [beat-ms 250
      base-line [:D3 :A2 :A#2 :C3
                 :D3 :F3 :G3 :C4
                 :D4 :A3 :F3 :C3
                 :D3 :F2 :G2 :A#2]
      num-measures (* 1 (count base-line))
      ]
  (stop)
  (letfn [(play-it
            ;; first, define a recursive note player
            ([interval instrument values]
             (play-it (now) interval instrument values 0))
            ([time interval instrument values counter]
             ;; the counter is only used for this hack:
             ;; because vintage bass "plays" forever...
             ;; close the instrument's play envelope.
             ;; you can try commenting this out and hear
             ;; what happens.
             (when (= (mod counter 4) 0)
               (ctl instrument :gate 0))
             (if-not (empty? values)
               (let [value (first values)
                     next-time (+ time interval)]
                 (when value
                   ;; at() is basically overtone's scheduler
                   (at time (instrument value)))
                 ;; after the note plays, schedule another call at the
                 ;; later time
                 (apply-at next-time
                           play-it [next-time interval instrument (rest values) (inc counter)])))))]

    ;; baseline plays once per 4 notes
    (play-it (* 4 beat-ms)
             synth/vintage-bass
             ;; cycle the base line forever (but in reality, just for
             ;; `num-measures` times)
             (take num-measures (cycle (map note base-line))))

    (play-it beat-ms
             piano
             ;; concat the sets-of-4-note-chords into a single
             ;; seq of notes to send to play-it
             (apply concat
                    (take num-measures
                          ;; for each root note, use overtone's rand-chord
                          ;; to construct a 4-note chord that spans up to
                          ;; 24 degrees
                          (map (fn [root-note] (rand-chord root-note :major 4 24))
                               (cycle base-line)))))))



;; ok, let's get back to more basics and build up to other exciting things.
;; first, let's add a few imports.

