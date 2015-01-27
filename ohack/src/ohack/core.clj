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

;; playing with the guitar synth

;; Mark found this guitar-pick function
(guitar-pick (guitar) 0 0) ;; String 0 (Low E) Fret 0 (open)

;; we need a function that will play a sequence of notes; notes will
;; contain an arbitrarily long sequence of pairs of numbers denoting
;; `string-index` `fret-value`
(defn guitar-pick-note-sequence
  "play a sequence of notes [string fret] on instrument instrument
  spaced by interval milliseconds

  interval = milliseconds between each play
  noteseq = [[string-index-1 fret-index-1
              string-index-2 fret-index-2 ... ]]
  "
  [interval noteseq]
  (let [playguitar (partial guitar-pick (guitar))
        timeseq (range (now) (+ (now) (* interval (count noteseq))) interval)]
    (doseq [[string-fret-seq timeval] (map vector noteseq timeseq)]
      (doseq [[string fret] (partition 2 string-fret-seq)]
        (playguitar string fret timeval)))))

(def playguitar320
  (partial guitar-pick-note-sequence 320))  ;; 320ms delay between picked strings

(def everybody-hurts
 "
  ;; Guitar Tab for Everybody Hurts by REM
  ;;
  ;;       D                        G
  ;; E:5]--------2-----------2------------3-----------3-----[
  ;; B:4]------3---2-------3---3--------0---0-------0---0---[
  ;; G:3]----2-------2---2-------2----0-------0---0-------0-[
  ;; D:2]--0-----------0------------------------------------[
  ;; A:1]---------------------------------------------------[
  ;; E:0]---------------------------3-----------3-----------[
")

;; now we can play multiple notes at the same time
(playguitar320 [[0 3, 3 0, 4 0, 5 3]])

;; we're going to fetch the tab from
;; http://tabs.ultimate-guitar.com/t/tracy_chapman/fast_car_ver8_tab.htm
(def fast-car-html (slurp "http://tabs.ultimate-guitar.com/t/tracy_chapman/fast_car_ver8_tab.htm"))
;; ok, it's working, now save the tab
(def fast-car-tab
  (-> fast-car-html
      (.split "Tabbed by")
      second
      (.split "hammer-on")
      first))

;; let's iteratively write a simple parser for this

;; first, filter out all lines that don't look like guitar lines
;; use map-index because we need to keep ordering information
(let [guitar-tab fast-car-tab

      index-line-list (filter
                       ;; a tab line must contain "-"
                       (fn [[_ s]] (.contains s "-"))
                       (into {} (map-indexed vector (s/split-lines guitar-tab))))

      ]
  ;; now we have all the indices of lines we care about
  ;; and we need to group them into blocks of 6 lines.
  ;; blocks of 6 lines must occur with consecutive line
  ;; indexes, so lets detect that.

  (loop [input (map first index-line-list)
         ;; use a buffer to store incoming consecutive indexes
         buf []
         rtn []]
    (if (empty? input)
      rtn
      (recur (rest input)
             buf
             (conj rtn (first input)))))
  )
