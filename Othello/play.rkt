#lang racket/gui

(require "AI-of-the-project.rkt")
(require "GUI-of-the-project.rkt")

(send reversi show #t)
(void (new message% [parent reversi] [label background]))