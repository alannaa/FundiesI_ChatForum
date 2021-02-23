;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Forum fixed|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

;To Run: (simple-net-forum 10010)

(define WIDTH 700)
(define HEIGHT 500)
(define NAME "pasco.a:3342")
(define SERVER "dictionary.ccs.neu.edu")
; font : String -> Image
;renders an image of a given string 
(define (font str)
  (text str 12 "black"))


;;-----------------------------------------------------------                                  
;                                  
;                                  
;   ;;;;;             ;            
;   ;    ;            ;            
;   ;     ;   ;;;   ;;;;;;    ;;;  
;   ;     ;  ;   ;    ;      ;   ; 
;   ;     ;      ;    ;          ; 
;   ;     ;  ;;;;;    ;      ;;;;; 
;   ;     ; ;    ;    ;     ;    ; 
;   ;     ; ;    ;    ;     ;    ; 
;   ;    ;  ;   ;;    ;     ;   ;; 
;   ;;;;;    ;;; ;     ;;;   ;;; ; 
;                                  
;A World is a Forum                                                                   
;A Forum is one of:
; - Viewall
; - Threadview
; - Newitem
; - Search
;INTERPRETATION : Represents four different "views" in a chat forum, where:
;A Viewall is a (make-viewall String History)
; INTERPRETATION: The user is viewing all posts (but not replies), 
;   and possibly typing a Command.
;A Threadview is a (make-threadview Post History)
; INTERPRETATION: The user is viewing a specific Post and its replies.
;A Newitem is a (make-newitem [Maybe Natural] String History)
; INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).  
;A Search is a (make-search String History)
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages.
#;(define (forum-temp f)
    (cond
      [(viewall? f) (viewall-temp f)]
      [(threadview? f) (threadview-temp f)]
      [(newitem? f) (newitem-temp f)]
      [(search? f) (search-temp f)]))

(define-struct viewall [command history])
(define-struct threadview [post history])
(define-struct newitem [id content history])
(define-struct search [type history])

 
(define-struct post [author content replies])
;A post is a structure: (make-post String String [List-of Reply])
;INTERPRETATION: a structure that contains the name of the author of a post, its content, and
; its replies

(define-struct reply [author content])
;A reply is a structure: (make-reply String String)
;INTERPRETATION: a structure that contains the name and author of a reply. 

;A Post is a (list Number (make-post String String [List-of Reply]))
;INTERPRETATION : The first item in the list represents the ID number of the Post, and the second is
; a structure that contains the contents of the post. 

;A History is a [List-of Post]
;INTERPRETATION : a comprehensive record of every post sent or recieved in the forum. 


;;Data regarding communication with the server:
;A ClientMsg is one of:
; - "CATCHUP"
; - (list "POST" String)
; - (list "REPLY" Natural String)
;INTERPRETATION
; - Sending the message "CATCHUP" tells the server to send over all the prior posts the server has
;    received. You may run program by asking for the server to catch up, however a second request
;    will restult in an error.
; - Senging the message (list "POST" String): a two-item list that indicates you are writing a
;    new post and contains the text of the post in the second item. 
; - Sending the message (list "REPLY" Natural String): a three-item list that indicates you are
;    replying to an existing post. The Number represents the original post's id, and the string is
;    text of the post.
#;(define (clientmsg-temp cm)
    (cond
      [(string? cm) ...]
      [(list? cm)
       (cond
         [(string=? (first cm) "POST") (... (second cm) ...)]
         [(string=? (first cm) "REPLY") (... (second cm) ... (third cm)...)])]))

;A ServerMsg is one of:
; - (list "POST" Natural String String)
; - (list "REPLY" Natural String String)
; - (list "ERROR" String)
;INTERPRETATION
; - Receiving a "POST" message: a new post with the post id number, author, and contents.
; - Receiving a "REPLY" message: there is a new reply containing the id number of the parent post,
;    the author of the reply, and the content of the reply message.
; - Receiving an "ERROR" message: the client made a mistake, the error message is the string. ;
#;(define (servermsg-temp sm)
    (cond
      [(string=? (first sm) "POST") (... (second sm) ... (third sm) ... (fourth sm) ...)]
      [(string=? (first sm) "REPLY") (... (second sm) ... (third sm) ... (fourth sm) ...)]
      [(string=? (first sm) "ERROR") (... (second sm) ...)]))


;An Error is one of:
; - #true
; - #false
;Where #false represents when the error canvas is open
;Where #true represents when the error canvas is closed

(define-struct erw [error return-state server-string])
;An Error World is a (make-erw Error Forum String)
;INTERPRETATION: a structure that holds the status of an error, the Forum that the
; error function will return once the error window is closed, and a string contatining the
; specific error message. 


;;===DATA EXAMPLES
;Example of a History:
(define a-history (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                        (list 2 (make-post "pasco.a" "week 2 homework" '()))
                        (list 1 (make-post "pasco.a" "week 1 homework" '()))))
(define a-history-with-reply (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                   (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                   (list 1 (make-post "pasco.a" "week 1 homework"
                                                      (list (make-reply "traynor.k"
                                                                        "i have a question."))))))
(define ex-smsg-post (list "POST" 4 "pasco.a" "week 4 homework"))
(define ex-smsg-reply (list "REPLY" 3 "traynor.k" "when is this due"))

;Viewall
(define va-type  (make-viewall "inserting a command" a-history))
(define va-with-reply (make-viewall "" a-history-with-reply))
(define va-catch (make-viewall "catchup" a-history))
(define va-new   (make-viewall "new" a-history))
(define va-reply (make-viewall "reply 1" a-history))
(define va-tview (make-viewall "view 1" a-history))

;Threadview
(define tv       (make-threadview (list 1 (make-post "pasco.a" "week 1 homework"
                                                     (list (make-reply "traynor.k"
                                                                       "i have a question."))))
                                  a-history-with-reply))
;Newitem
(define ni-post  (make-newitem #false "there is a new assignment" a-history))
(define ni-reply (make-newitem 1 "i have a question re hw 1" a-history))

;Search
(define S-empty    (make-search "" a-history))
(define S-hello    (make-search "hello" a-history))
(define S-backspce (make-search "hell" a-history))
(define S-add-ltrs (make-search "hellos" a-history))
(define S-results  (make-search "essay" a-history))
(define S-switch-mode (make-viewall "" a-history))
;;-----------------------------------------------------------                                  
;                                  
;                      ;           
;  ;;   ;;             ;           
;  ;;   ;;                    
;  ;;; ;;;    ;;;    ;;;    ; ;;;  
;  ; ; ; ;   ;   ;     ;    ;;   ; 
;  ; ; ; ;       ;     ;    ;    ; 
;  ;  ;  ;   ;;;;;     ;    ;    ; 
;  ;  ;  ;  ;    ;     ;    ;    ; 
;  ;     ;  ;    ;     ;    ;    ; 
;  ;     ;  ;   ;;     ;    ;    ; 
;  ;     ;   ;;; ;  ;;;;;;; ;    ; 
;                                  
;                                                                  
; simple-net-forum : Number -> Forum
;Runs the simple-net-forum program given a port number to connect to
;To Run: (simple-net-forum 10010)
(define (simple-net-forum p)
  (big-bang (make-viewall "" '())
            [port p]
            [name NAME]
            [register SERVER]
            [to-draw draw-forum]
            [on-key keys]
            [on-receive receive-word]))
                                 
;                                  
;                                  
;   ;;;;;                          
;   ;    ;                         
;   ;     ;  ;;;;;    ;;;  ;      ;
;   ;     ;  ;;      ;   ; ;      ;
;   ;     ;  ;           ; ;     ; 
;   ;     ;  ;       ;;;;;  ; ;; ; 
;   ;     ;  ;      ;    ;  ; ;; ; 
;   ;     ;  ;      ;    ;  ;;;;;; 
;   ;    ;   ;      ;   ;;  ;;  ;; 
;   ;;;;;    ;       ;;; ;   ;  ;  
;                                  
;                                 
; draw-forum : Forum -> Image
;;Renders the images of each mode; Viewall, Threadview, Newitem or Search
(check-expect (draw-forum va-type)
              (overlay
               (above/align "left"
                            (frame (font "inserting a command"))
                            (font "3 pasco.a: essay assignment")
                            (font "2 pasco.a: week 2 homework")
                            (font "1 pasco.a: week 1 homework"))
               (overlay/xy
                (above/align "left"
                             (text "Options:" 12 "black")
                             (text "Enter 'new' to create a new post." 12 "black")
                             (text
                              "Enter 'reply' and a post-id number to reply to an existing post."
                              12 "black")
                             (text
                              "Enter 'view' and a post-id number to view a post and its replies."
                              12 "black")
                             (text "Enter 'catchup' to refresh history." 12 "black")
                             (text "Press F2 at any time to search." 12 "black")
                             (text "Press F1 at any time to view all again." 12 "black"))
                -30 -50 (overlay/xy
                         (text "View All" 18 "lightblue") -30 -30
                         (rectangle WIDTH HEIGHT "solid" "white")))))
(check-expect (draw-forum va-with-reply)
              (overlay
               (above/align "left"
                            (frame (font ""))
                            (font "3 pasco.a: essay assignment")
                            (font "2 pasco.a: week 2 homework")
                            (font "1 pasco.a: week 1 homework"))
               (overlay/xy
                (above/align "left"
                             (text "Options:" 12 "black")
                             (text "Enter 'new' to create a new post." 12 "black")
                             (text
                              "Enter 'reply' and a post-id number to reply to an existing post."
                              12 "black")
                             (text
                              "Enter 'view' and a post-id number to view a post and its replies."
                              12 "black")
                             (text "Enter 'catchup' to refresh history." 12 "black")
                             (text "Press F2 at any time to search." 12 "black")
                             (text "Press F1 at any time to view all again." 12 "black"))
                -30 -50 (overlay/xy
                         (text "View All" 18 "lightblue") -30 -30
                         (rectangle WIDTH HEIGHT "solid" "white")))))
(check-expect (draw-forum tv) (overlay
                               (above/align "left"
                                            (font "1 pasco.a: week 1 homework")
                                            (font "--traynor.k: i have a question."))
                               (overlay/xy (text "Thread View" 18 "lightblue") -30 -30
                                           (rectangle WIDTH HEIGHT "solid" "white"))))
(check-expect (draw-forum ni-post) (overlay
                                    (above/align "left"
                                                 (frame (font "there is a new assignment"))
                                                 empty-image)
                                    (overlay/xy (text "New Post" 18 "lightblue") -30 -30
                                                (rectangle WIDTH HEIGHT "solid" "white"))))
(check-expect (draw-forum ni-reply) (overlay
                                     (above/align "left"
                                                  (font "1 pasco.a: week 1 homework")
                                                  (frame (font "i have a question re hw 1")))
                                     (overlay/xy (text "Reply" 18 "lightblue") -30 -30
                                                 (rectangle WIDTH HEIGHT "solid" "white"))))
(check-expect (draw-forum S-empty) (overlay
                                    (above/align "left"
                                                 (frame (font ""))
                                                 empty-image)
                                    (overlay/xy (text "Search" 18 "lightblue") -30 -30
                                                (rectangle WIDTH HEIGHT "solid" "white"))))
(check-expect (draw-forum S-results) (overlay
                                      (above/align "left"
                                                   (frame (font "essay"))
                                                   (font "3 pasco.a: essay assignment"))
                                      (overlay/xy (text "Search" 18 "lightblue") -30 -30
                                                  (rectangle WIDTH HEIGHT "solid" "white"))))

(define (draw-forum f)
  (local ((define VATITLE (text "View All" 18 "lightblue"))
          (define TVTITLE (text "Thread View" 18 "lightblue"))
          (define NITITLE-P (text "New Post" 18 "lightblue"))
          (define NITITLE-R (text "Reply" 18 "lightblue"))
          (define STITLE (text "Search" 18 "lightblue"))
          ; background : Image -> Image
          ;;Draws the background image of each mode with its title
          ;;For Example: (background STITLE) will produce an image of a blank white canvas with the
          ;;  word "Search" written in lightblue in the top left corner.
          (define (background title)
            (overlay/xy title -30 -30 (rectangle WIDTH HEIGHT "solid" "white")))
          ; va-background : Image -> Image
          ;;Draws a background image with directions while in viewall mode.
          ;;For Example: (va-background VATITLE) will produce a background image with directions
          ;;  of what the user may type to control the forum.
          (define (va-background title)
            (overlay/xy
             (above/align "left"
                          (text "Options:" 12 "black")
                          (text "Enter 'new' to create a new post." 12 "black")
                          (text "Enter 'reply' and a post-id number to reply to an existing post."
                                12 "black")
                          (text
                           "Enter 'view' and a post-id number to view a post and its replies."
                           12 "black")
                          (text "Enter 'catchup' to refresh history." 12 "black")
                          (text "Press F2 at any time to search." 12 "black")
                          (text "Press F1 at any time to view all again." 12 "black"))
             -30 -50 (background title))))
    
    (cond
      [(viewall? f) (overlay
                     (above/align "left"
                                  (frame (font (viewall-command f)))
                                  (foldr render-post empty-image (viewall-history f)))
                     (va-background VATITLE))]
      [(threadview? f) (overlay
                        (above/align "left"
                                     (render-post (threadview-post f) empty-image)
                                     (foldl draw-replies empty-image
                                            (post-replies (second (threadview-post f)))))
                        (background TVTITLE))]
      [(newitem? f)
       (cond
         [(boolean? (newitem-id f)) (overlay
                                     (above/align "left"
                                                  (frame (font (newitem-content f)))
                                                  empty-image)
                                     (background NITITLE-P))]
         [(number? (newitem-id f))
          (overlay
           (above/align "left"
                        (local (; extract-post : History -> Post
                                ;;Searches the History and returns only the post with which
                                ;;the user is replying to. For Example: if the newitem-id is
                                ;;1, extract-post will return only post number 1 in the given
                                ;;history.
                                (define (extract-post h)
                                  (cond
                                    [(equal? (newitem-id f) (first (first h))) (first h)]
                                    [else (extract-post (rest h))])))
                          (render-post (extract-post (newitem-history f)) empty-image)) 
                        (frame (font (newitem-content f))))
           (background NITITLE-R))])]
      [(search? f)
       (cond
         [(string=? (search-type f) "") (background STITLE)]
         [else (overlay
                (above/align "left"
                             (frame (font (search-type f)))
                             (foldr render-post empty-image (filter-history
                                                             (search-type f)
                                                             (search-history f))))
                (background STITLE))])])))


;;---
; render-post : Post Image -> Image
;;Renders an image of a single Post, without its replies 
(check-expect (render-post (list 1 (make-post "pasco.a" "week 1 homework" '())) empty-image)
              (above/align "left" (font "1 pasco.a: week 1 homework") empty-image))
(check-expect (render-post (list 1 (make-post "pasco.a" "essay assignment"
                                              (list (make-reply "traynor.k" "when is this due?"))))
                           empty-image)
              (above/align "left" (font "1 pasco.a: essay assignment") empty-image))

(define (render-post p i)
  (above/align "left"
               (font (string-append (number->string (first p)) " "
                                    (post-author (second p)) ": "
                                    (post-content (second p)))) i))

; draw-replies : Reply Image -> Image
;;Adds a Reply to an Image of a Message
(check-expect (draw-replies (make-reply "traynor.k" "when is this due?") empty-image)
              (above/align "left" (font "--traynor.k: when is this due?") empty-image))
(check-expect (draw-replies (make-reply "traynor.k" "nvm")
                            (above/align "left"
                                         (font "--traynor.k: when is this due?") empty-image))
              (above/align "left"
                           (font "--traynor.k: when is this due?")
                           (font "--traynor.k: nvm")))
                             
(define (draw-replies r i)
  (above/align "left"
               i
               (font (string-append "--" (reply-author r) ": " (reply-content r)))))

; filter-history : String History -> History
;;creates a list of all items in the history that include the search string
(check-expect (filter-history "no" (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                         (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                         (list 1 (make-post "pasco.a" "week 1 homework" '())))) '())
(check-expect (filter-history "" (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                       (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                       (list 1 (make-post "pasco.a" "week 1 homework" '())))) '())
(check-expect (filter-history "we" (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                         (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                         (list 1 (make-post "pasco.a" "week 1 homework" '()))))
              (list (list 2 (make-post "pasco.a" "week 2 homework" '()))
                    (list 1 (make-post "pasco.a" "week 1 homework" '()))))

(define (filter-history str h)
  (if (string=? str "")
      '()
      (local (; contains-search-string? : Post -> Boolean
              ;;Determines whether a search string appears in an item on the History
              ;;For Example: if the content of the post is "hello" and the search string is "he"
              ;;             result will be #t. If the search string is "a" the result will be #f. 
              (define (contain-search-string? post)
                (string-contains? (post-content (second post)) str)))
        (filter contain-search-string? h))))


;                                  
;                                  
;                                  
;   ;    ;                         
;   ;   ;                          
;   ;  ;     ;;;;   ;    ;   ;;;;  
;   ; ;;     ;  ;;   ;   ;  ;    ; 
;   ;;;     ;    ;   ;  ;   ;      
;   ;; ;    ;;;;;;   ;  ;   ;;;    
;   ;  ;;   ;         ; ;      ;;; 
;   ;   ;   ;         ;;         ; 
;   ;    ;   ;        ;;    ;    ; 
;   ;    ;;  ;;;;;     ;     ;;;;  
;                      ;           
;                     ;            
;                    ;;            
;
; keys : Forum KeyEvent -> Forum 
;; - Adds characters to what user is typing
;; - Deletes last character of what user is typing
;; - Sends commands to the server
;; - F2 always returns Search mode
;; - F1 always returns Viewall mode
(check-expect (keys va-type "s") (make-viewall "inserting a commands" a-history))
(check-expect (keys va-type "\b") (make-viewall "inserting a comman" a-history))
(check-expect (keys va-catch "\r") (make-package (make-viewall "" a-history) "CATCHUP"))
(check-expect (keys va-tview "\r") (make-threadview (list 1 (make-post "pasco.a"
                                                                       "week 1 homework" '()))
                                                    a-history))
(check-expect (keys va-new "\r") (make-newitem #false "" a-history))
(check-expect (keys va-reply "\r") (make-newitem 1 "" a-history))
(check-expect (keys va-type "f2") (make-search "" a-history))

(check-expect (keys tv "s") tv)
(check-expect (keys tv "\b") tv)
(check-expect (keys tv "\r") tv)
(check-expect (keys tv "f1") (make-viewall "" a-history-with-reply))
(check-expect (keys tv "f2") (make-search "" a-history-with-reply))

(check-expect (keys ni-post "a") (make-newitem #false "there is a new assignmenta" a-history))
(check-expect (keys ni-post "\b") (make-newitem #false "there is a new assignmen" a-history))
(check-expect (keys ni-post "\r")
              (make-package (make-viewall
                             ""
                             (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                   (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                   (list 1 (make-post "pasco.a" "week 1 homework" '()))))
                            (list "POST" "there is a new assignment")))
(check-expect (keys ni-post "f1") (make-viewall "" a-history))
(check-expect (keys ni-post "f2") (make-search "" a-history))

(check-expect (keys ni-reply "a") (make-newitem 1 "i have a question re hw 1a" a-history))
(check-expect (keys ni-reply "\b") (make-newitem 1 "i have a question re hw " a-history))
(check-expect (keys ni-reply "\r")
              (make-package (make-viewall
                             ""
                             (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                   (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                   (list 1 (make-post "pasco.a" "week 1 homework" '()))))
                            (list "REPLY" 1 "i have a question re hw 1")))
(check-expect (keys ni-reply "f1") (make-viewall "" a-history))
(check-expect (keys ni-reply "f2") (make-search "" a-history))

(check-expect (keys S-hello "s") S-add-ltrs)
(check-expect (keys S-hello "\b") S-backspce)
(check-expect (keys S-hello "\r") S-hello)
(check-expect (keys S-results "f1") S-switch-mode)

(define (keys f ke)
  (local (; apply-keys : Forum KeyEvent -> Forum
          ;finds the given key event and applies the proper result helper function to the forum
          ;;For Example: if the backspace key is pressed, apply-keys will call the backspace
          ;;             function on the current forum. 
          (define (apply-keys f ke)
            (cond
              [(key=? "\b" ke) (backspace f)]
              [(key=? "\r" ke) (return f)]
              [(key=? "f1" ke) (switch f ke)]
              [(key=? "f2" ke) (switch f ke)]
              [(= (string-length ke) 1) (add-letters f ke)]
              [else f])))
    (cond
      [(viewall? f) (apply-keys f ke)]
      [(threadview? f) (apply-keys f ke)]
      [(newitem? f) (apply-keys f ke)]
      [(search? f) (apply-keys f ke)])))



; add-letters : Forum KeyEvent -> Forum
;;allows the user to type messages by appending a series of 1strings
(check-expect (add-letters va-type "s") (make-viewall "inserting a commands" a-history))
(check-expect (add-letters ni-post "a") (make-newitem #false "there is a new assignmenta"
                                                      a-history))
(check-expect (add-letters ni-reply "a") (make-newitem 1 "i have a question re hw 1a"
                                                       a-history))
(check-expect (add-letters S-hello "s") S-add-ltrs)

(define (add-letters f ke)
  (cond
    [(viewall? f) (make-viewall (string-append (viewall-command f) ke) (viewall-history f))]
    [(newitem? f) (make-newitem (newitem-id f)
                                (string-append (newitem-content f) ke) (newitem-history f))]
    [(search? f) (make-search (string-append (search-type f) ke) (search-history f))]
    [else f]))

; backspace : Forum -> Forum
;;deletes one character from what user is currently typing
(check-expect (backspace (make-viewall "" a-history)) (make-viewall "" a-history))
(check-expect (backspace va-type) (make-viewall "inserting a comman" a-history))
(check-expect (backspace tv) tv)
(check-expect (backspace ni-post) (make-newitem #false "there is a new assignmen" a-history))
(check-expect (backspace ni-reply) (make-newitem 1 "i have a question re hw " a-history))
(check-expect (backspace (make-viewall "" a-history)) (make-viewall "" a-history))

(define (backspace f)
  (cond
    [(viewall? f)
     (cond
       [(string=? "" (viewall-command f)) f]
       [else (make-viewall (substring (viewall-command f)
                                      0
                                      (- (string-length (viewall-command f)) 1))
                           (viewall-history f))])]
    [(newitem? f)
     (cond
       [(string=? "" (newitem-content f)) f]
       [else (make-newitem (newitem-id f)
                           (substring (newitem-content f)
                                      0
                                      (- (string-length (newitem-content f)) 1))
                           (newitem-history f))])]
    [(search? f)
     (cond
       [(string=? "" (search-type f)) f]
       [else (make-search (substring (search-type f)
                                     0
                                     (- (string-length (search-type f)) 1))
                          (search-history f))])]
    [else f]))

; return : Forum -> [or Forum Package]
;;sends current message to the server and clears the current message box
(check-expect (return va-catch) (make-package (make-viewall "" a-history) "CATCHUP"))
(check-expect (return va-tview) (make-threadview (list 1 (make-post "pasco.a"
                                                                    "week 1 homework"
                                                                    '()))
                                                 a-history))
(check-expect (return va-new) (make-newitem #false "" a-history))
(check-expect (return va-reply) (make-newitem 1 "" a-history))
(check-expect (return tv) tv)
(check-expect (return ni-post)
              (make-package (make-viewall
                             ""
                             (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                   (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                   (list 1 (make-post "pasco.a" "week 1 homework" '()))))
                            (list "POST" "there is a new assignment")))
(check-expect (return ni-reply)
              (make-package (make-viewall
                             ""
                             (list (list 3 (make-post "pasco.a" "essay assignment" '()))
                                   (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                   (list 1 (make-post "pasco.a" "week 1 homework" '()))))
                            (list "REPLY" 1 "i have a question re hw 1")))
(check-expect (return S-hello) S-hello)

(define (return f)
  (cond
    [(viewall? f)
     (local (; process-command : String -> [or Newitem Threadview Viewall]
             ;;processes the commands entered in Viewall mode
             (define (process-command str)
               (cond
                 [(string=? str "") f]
                 [(string=? str "catchup")
                  (make-package (make-viewall "" (viewall-history f))
                                "CATCHUP")]
                 [(string=? str "new")
                  (make-newitem #false "" (viewall-history f))]
                 [(string=? (first (string-split str)) "reply")
                  (make-newitem (string->number (second (string-split str)))
                                "" (viewall-history f))]
                 [(string=? (first (string-split str)) "view")
                  (local (; return-thread : String History -> [or Post Boolean]
                          ;;locates and returns the item in the history with the given id number
                          (define (return-thread str h)
                            (cond
                              [(empty? h) #false]
                              [else (if (= (string->number (second (string-split str)))
                                           (first (first h)))
                                        (first h)
                                        (return-thread str (rest h)))])))
                    (cond
                      [(equal? (return-thread str (viewall-history f))
                               #false)
                       (erw-return-state
                        (error-window (make-erw
                                       #false
                                       (make-viewall "" (viewall-history f))
                                       "This thread does not exist.")))]
                      [else
                       (make-threadview (return-thread (viewall-command f) (viewall-history f))
                                        (viewall-history f))]))]
                 [else f])))
       (process-command (viewall-command f)))]
    [(threadview? f) f]
    [(newitem? f)
     (cond
       [(boolean? (newitem-id f)) (make-package (make-viewall "" (newitem-history f))
                                                (list "POST" (newitem-content f)))]
       [(number? (newitem-id f)) (make-package (make-viewall "" (newitem-history f))
                                               (list "REPLY" (newitem-id f) (newitem-content f)))])]
    [(search? f) f]))

; switch : Forum -> Forum
;; - F2 always returns Search mode
;; - F1 always returns Viewall mode
(check-expect (switch va-type "f2") S-empty)
(check-expect (switch va-type "f1") va-type)
(check-expect (switch tv "f2") (make-search "" a-history-with-reply))
(check-expect (switch tv "f1") (make-viewall "" a-history-with-reply))
(check-expect (switch ni-post "f2") S-empty)
(check-expect (switch ni-reply "f2") S-empty)
(check-expect (switch S-empty "f1") (make-viewall "" a-history))
(check-expect (switch S-empty "f2") S-empty)

(define (switch f ke)
  (cond
    [(string=? ke "f1")
     (cond
       [(viewall? f) f]
       [(threadview? f) (make-viewall "" (threadview-history f))]
       [(newitem? f) (make-viewall "" (newitem-history f))]
       [(search? f) (make-viewall "" (search-history f))])]
    [(string=? ke "f2")
     (cond
       [(viewall? f) (make-search "" (viewall-history f))]
       [(threadview? f) (make-search "" (threadview-history f))]
       [(newitem? f) (make-search "" (newitem-history f))]
       [(search? f) f])]))

;                                                          
;                                                          
;                                      ;                   
;   ;;;;;;                             ;                   
;   ;    ;;                                                
;   ;     ;  ;;;;     ;;;    ;;;;    ;;;    ;    ;   ;;;;  
;   ;     ;  ;  ;;   ;   ;   ;  ;;     ;    ;    ;   ;  ;; 
;   ;    ;; ;    ;  ;       ;    ;     ;     ;  ;   ;    ; 
;   ;;;;;   ;;;;;;  ;       ;;;;;;     ;     ;  ;   ;;;;;; 
;   ;    ;  ;       ;       ;          ;     ;  ;   ;      
;   ;     ; ;       ;       ;          ;      ;;    ;      
;   ;     ;  ;       ;   ;   ;         ;      ;;     ;     
;   ;      ; ;;;;;    ;;;    ;;;;;  ;;;;;;;   ;;     ;;;;; 
;                                                          
;
; receive-word : Forum ServerMsg -> Forum
;;Receives messages from the server as a ServerMsg and processes them into Posts
;;Processes an error message in a new window if an error is sent by the server
(check-expect (receive-word va-type ex-smsg-post)
              (make-viewall "inserting a command"
                            (list (list 4 (make-post "pasco.a" "week 4 homework" '()))
                                  (list 3 (make-post "pasco.a" "essay assignment" '()))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word tv ex-smsg-post)
              (make-threadview
               (list 1 (make-post "pasco.a" "week 1 homework"
                                  (list (make-reply "traynor.k" "i have a question."))))
               (list (list 4 (make-post "pasco.a" "week 4 homework" '()))
                     (list 3 (make-post "pasco.a" "essay assignment" '()))
                     (list 2 (make-post "pasco.a" "week 2 homework" '()))
                     (list 1 (make-post "pasco.a" "week 1 homework"
                                        (list (make-reply "traynor.k"
                                                          "i have a question.")))))))
(check-expect (receive-word ni-post ex-smsg-post)
              (make-newitem #false "there is a new assignment"
                            (list (list 4 (make-post "pasco.a" "week 4 homework" '()))
                                  (list 3 (make-post "pasco.a" "essay assignment" '()))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word ni-reply ex-smsg-post)
              (make-newitem 1 "i have a question re hw 1"
                            (list (list 4 (make-post "pasco.a" "week 4 homework" '()))
                                  (list 3 (make-post "pasco.a" "essay assignment" '()))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word S-hello ex-smsg-post)
              (make-search "hello" (list (list 4 (make-post "pasco.a" "week 4 homework" '()))
                                         (list 3 (make-post "pasco.a" "essay assignment" '()))
                                         (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                         (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word va-type ex-smsg-reply)
              (make-viewall "inserting a command"
                            (list (list 3 (make-post "pasco.a" "essay assignment"
                                                     (list
                                                      (make-reply "traynor.k" "when is this due"))))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word tv ex-smsg-reply)
              (make-threadview
               (list 1 (make-post "pasco.a" "week 1 homework"
                                  (list (make-reply "traynor.k" "i have a question."))))
               (list (list 3 (make-post "pasco.a" "essay assignment"
                                        (list
                                         (make-reply "traynor.k" "when is this due"))))
                     (list 2 (make-post "pasco.a" "week 2 homework" '()))
                     (list 1 (make-post "pasco.a" "week 1 homework"
                                        (list (make-reply "traynor.k" "i have a question.")))))))
(check-expect (receive-word ni-post ex-smsg-reply)
              (make-newitem #false "there is a new assignment"
                            (list (list 3 (make-post "pasco.a" "essay assignment"
                                                     (list
                                                      (make-reply "traynor.k" "when is this due"))))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word ni-reply ex-smsg-reply)
              (make-newitem 1 "i have a question re hw 1"
                            (list (list 3 (make-post "pasco.a" "essay assignment"
                                                     (list
                                                      (make-reply "traynor.k" "when is this due"))))
                                  (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                  (list 1 (make-post "pasco.a" "week 1 homework" '())))))
(check-expect (receive-word S-hello ex-smsg-reply)
              (make-search "hello" (list (list 3 (make-post "pasco.a" "essay assignment"
                                                            (list
                                                             (make-reply "traynor.k"
                                                                         "when is this due"))))
                                         (list 2 (make-post "pasco.a" "week 2 homework" '()))
                                         (list 1 (make-post "pasco.a" "week 1 homework" '())))))

(define (receive-word f sm)
  (cond
    [(string=? "POST" (first sm))
     (local (; post-creator : ServerMsg -> Post
             ;;processes the server message into a Post
             ;;For Example: If the ServerMsg is (list "POST" 2 "pasco.a" "week 2 hw")
             ;;             post-creator returns (list 2 (make-post "pasco.a" "week 2 hw" '()))
             (define (post-creator sm)
               (list (second sm) (make-post (third sm) (fourth sm) '()))))
       (cond
         [(viewall? f) (make-viewall (viewall-command f) (cons (post-creator sm)
                                                               (viewall-history f)))]
         [(threadview? f) (make-threadview (threadview-post f) (cons (post-creator sm)
                                                                     (threadview-history f)))]
         [(newitem? f) (make-newitem (newitem-id f) (newitem-content f) (cons (post-creator sm)
                                                                              (newitem-history f)))]
         [(search? f) (make-search (search-type f) (cons (post-creator sm)
                                                         (search-history f)))]))]
    [(string=? "REPLY" (first sm))
     (local (; same-id? : Post Post -> Boolean
             ;;determines whether two Posts have the same ID#.
             ;;For Example: (list 1 (make-post "traynor.k" "hello" '())) and
             ;;             (list 1 (make-post "traynor.k" "bye" '()))) return #t.
             ;;         or: (list 2 (make-post "traynor.k" "hello" '())) and
             ;;             (list 1 (make-post "traynor.k" "bye" '()))) return #f.
             (define (same-id? p1 p2)
               (= (first p1) (first p2)))
             ; replace-post : History Post -> History
             ;;finds the Post in the given history with the same ID# as the given Post
             ;; and replaces it with the given post.
             ;;For Example: ?????
             (define (replace-post h p)
               (cond
                 [(empty? h) h]
                 [(cons? h) (if (same-id? p (first h))
                                (cons p (rest h))
                                (cons (first h) (replace-post (rest h) p)))]))
             ; add-reply : History ServerMsg -> History
             ;;Adds a reply to the History in the post with the matching ID#.
             ;;For Example: ?????
             (define (add-reply h r)
               (local (;;This variable returns the post within the history that has the id# of
                       ;; the reply
                       (define assocv (assoc (second r) h)))
                 (cond
                   [(equal? #f assocv) h]
                   [else (replace-post h (list (first assocv)
                                               (make-post (post-author (second assocv))
                                                          (post-content (second assocv))
                                                          (cons (make-reply (third r) (fourth r))
                                                                (post-replies
                                                                 (second assocv))))))]))))
       (cond
         [(viewall? f) (make-viewall (viewall-command f) (add-reply (viewall-history f) sm))]
         [(threadview? f) (make-threadview (threadview-post f)
                                           (add-reply (threadview-history f) sm))]
         [(newitem? f) (make-newitem (newitem-id f) (newitem-content f)
                                     (add-reply (newitem-history f) sm))]
         [(search? f) (make-search (search-type f) (add-reply (search-history f) sm))]))]
    [(string=? "ERROR" (first sm))
     (cond
       [(viewall? f) (erw-return-state (error-window
                                        (make-erw #false (make-viewall "" (viewall-history f))
                                                  (second sm))))]
       [(threadview? f) (erw-return-state (error-window
                                           (make-erw #false (make-threadview (threadview-post f)
                                                                             (threadview-history f))
                                                     (second sm))))]
       [(newitem? f) (erw-return-state (error-window
                                        (make-erw #false (make-newitem (newitem-id f)
                                                                       ""
                                                                       (newitem-history f))
                                                  (second sm))))]
       [(search? f) (erw-return-state (error-window
                                       (make-erw #false (make-search "" (search-history f))
                                                 (second sm))))])]))


; error-window : ErrorWorld -> ErrorWorld
;;Opens a new window that contains the contents of an error message sent by the server
(define (error-window erw)
  (local (; draw-error : String -> Image
          ;;Draws an image of the text of an error message
          ;;For Example: ?????
          (define (draw-error smsg)
            (overlay (font (string-append "Error: " (erw-server-string smsg)))
                     (empty-scene 250 100))))
    (big-bang erw
              [to-draw draw-error])))
