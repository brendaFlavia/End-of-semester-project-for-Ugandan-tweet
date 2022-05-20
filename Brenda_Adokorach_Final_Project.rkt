#lang racket
(require data-science-master)
(require csv-reading)
(require plot)
(require math)

(define file-path "Uganda_tweet_Jan_Dec.csv")

(define (read-csv-with-piped-delimiter file-path
		  #:->number? [->number? #f]
		  #:header? [header? #t]
                  )
  (let ((csv-read (make-csv-reader-maker
                     '((separator-chars #\|)
                       (comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp_data (csv->list (csv-read (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (if header?
		  (cons (car tmp_data) (map (lambda (x) (map string->number x)) (cdr tmp_data)))
		  (map (lambda (x) (map string->number x)) tmp_data))
	      ;; Else, leave everything as strings
	      tmp_data))))))



(define data (read-csv-with-piped-delimiter file-path 
                       
                       )
  )

(define tweet_data
  (let ([tmp_data (map (λ (x) (list (list-ref x 0))) data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp_data)))
(define t_sample_data (flatten tweet_data))

(define all_tweets-text (apply string-append t_sample_data))


(define tweet-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase all_tweets-text) #:websafe? #t)))



(define tweet_words (document->tokens tweet-text #:sort? #t))


(define sentiment (list->sentiment tweet_words #:lexicon 'nrc))

 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Tweet moods"
	  #:y-label "Frequency")))

(define sentiment_bing (list->sentiment tweet_words #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_bing 'sentiment) ($ sentiment_bing 'freq))
	 #:y-min 0
	 #:y-max 100000
	 #:invert? #f
	 #:color "MediumOrchid"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))

