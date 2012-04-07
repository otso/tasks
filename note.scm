":"; exec racket -r $0 "$@"

;;;; A simple task/note application
;;;; - uses sqlite, creates "notes.db" file if it does not exist
;;;;
;;;; TODO
;;;; - enable copy-paste for the input field
;;;; - edit individual tasks with a double click - move to edit mode
;;;; - remove debug prints

;;; helpers

; get current time
(require racket/date)
(date-display-format 'iso-8601)
(define (current-time-string)
  (date->string (current-date)
                [time #t]))

;;; db

(require db)

(struct tasks (db))
(define notes-db-name "notes.db")

;; set up db
(define (initialize-tasks! name)
  (define db (sqlite3-connect #:database name #:mode 'create))
  (define all-tasks (tasks db))
  (unless (table-exists? db "tasks")
    (query-exec db
     (string-append
      "CREATE TABLE tasks "
      "(id INTEGER PRIMARY KEY, task TEXT, taskdone INTEGER, 
      datecreated TEXT, dateclosed TEXT)"))
  all-tasks))

(initialize-tasks! notes-db-name)

(define db (sqlite3-connect #:database notes-db-name))
(define all-tasks (tasks db))

;; insert to db
(define (tasks-insert-task! task)
  (query-exec
    (tasks-db all-tasks)
    "INSERT INTO tasks (task, taskdone, datecreated) VALUES (?, ?, ?)"
    task 0 (current-time-string)))

;; query for the tasks
(define (query-tasks-not-done)
  (query-rows
   (tasks-db all-tasks)
   "SELECT * FROM tasks WHERE taskdone < 1"))

;; mark task done
(define mark-task-done!
  (lambda (task-id)
    (query-exec
      (tasks-db all-tasks)
      "UPDATE tasks SET taskdone=1, dateclosed=? WHERE id=?" 
              (current-time-string) task-id)))

;; query the amount of tasks open
(define (query-amount-tasks)
  (query-value
   (tasks-db all-tasks)
   "SELECT COUNT(*) FROM tasks WHERE taskdone=0"))

;;; ui

(require racket/class
         racket/gui/base)

(define notes-list '())

(define note-frame (new frame% [label "Tasks"]
                      [width 300]
                      [height 300]
                      [stretchable-width #f] ; no default spacing
                      [stretchable-height #f] ; no default spacing
                      [alignment '(center center)]))

(define upper-panel (new horizontal-panel% [parent note-frame]
                                     [alignment '(center center)]))

;; task edit field
(define note-canvas (new editor-canvas% [parent upper-panel]
                         [style '(hide-vscroll hide-hscroll no-border)] ;transparent)]
                         [min-width 500]
                         [line-count 1]))
(define note-text (new text%))
(send note-canvas set-editor note-text)
(send note-canvas force-display-focus #t)

(define upper-right-panel (new vertical-panel% [parent upper-panel]
                                     [alignment '(center center)]))

;; add new task button
(define new-task (new button% [parent upper-right-panel]
                     [label "Add"]
                     (callback (lambda (button event)
                                 (when (not (string=? "" (send note-text get-flattened-text)))
                                   (tasks-insert-task! (send note-text get-flattened-text))
                                   (send msg set-label "Task added")
                                   (populate-task-list!)
                                   (send note-text select-all)
                                   (send note-text clear))))))

(define bottom-panel (new horizontal-panel% [parent note-frame]
                                     [alignment '(center center)]))

;; task list
(define notes-list-box (new list-box% [parent bottom-panel]
                      [label "Tasks: "]
                      [style '(single)] ; multiple for multiedit
                      [choices notes-list]
                      [min-width 500]
                      [callback (lambda (list-box event)
                          (display
                            (format "~a@~a: ~a~%"
                                  (send event get-event-type)
                                  (send event get-time-stamp)
                                  (send list-box get-selections))))]))

(define bottom-right-panel (new vertical-panel% [parent bottom-panel]
                                     [alignment '(center center)]))

;; mark done
(define note-mark-done (new button% [parent bottom-right-panel]
                     [label "Mark done"]
                     (callback (lambda (button event)
                                 (send msg set-label "Task marked done")
                                 (display 
                                   (format "Mark done item ~a" 
                                           (send notes-list-box get-selection)))
                                 (newline)
                                 (mark-task-done! (send notes-list-box get-data 
                                                       (send notes-list-box get-selection)))
                                 (populate-task-list!)))))

;; edit task
(define note-edit (new button% [parent bottom-right-panel]
             [label "Fork"]
             ; Callback procedure for a button click:
             (callback (lambda (button event)
                         (send msg set-label "Forking...")
                         (send note-text select-all)
                         (send note-text clear)
                         (send note-text insert (send notes-list-box get-string 
                                                      (send notes-list-box get-selection)))))))

;; events text
(define msg (new message% [parent note-frame]
                          [label (format "Open tasks: ~a" (query-amount-tasks))]))

;; clear and populate the UI with the tasks from db
(define (populate-task-list!)
  (send notes-list-box clear)
  (map (lambda (task-row)
    (display (vector-ref task-row 1))
    (newline)
    (send notes-list-box append (format "~a" 
      (vector-ref task-row 1))
      ;(vector-ref task-row 3)) ; the date the task was created 
      (vector-ref task-row 0))) ; listbox data deposits the row id
    (query-tasks-not-done)))

;;; main

(populate-task-list!)
(send note-frame show #t)

