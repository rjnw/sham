#lang scribble/doc

@(require scribble/manual)

@title{The Sham Reference}

@defstruct*[sham:def ([info anything?] [id sham:ast:terminal:sym?])]
@defstruct*[(sham:def:module sham:def) ([defs (list? sham:def?)])]
@(racketgrammar*
  [sham:def
    sham:def:module
    sham:def:function
    sham:def:type
    sham:def:global
    sham:def:global-string]
  [sham:def:module (info sham:def ...)]
  [sham:def:function ((sham:ast:type sham:ast:type) ... sham:ast:type sham:ast:stmt)]
  [def
    (sham:def:module def ...)
    (sham:def:function ((arg-ids arg-types) ... ret-type body-stmt))]
  [arg-ids terminal.sym]
  [arg-types type]
  [type
   (internal)
   (ref terminal.sym)
   (struct ((fields types) ...))])
