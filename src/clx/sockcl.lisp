;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;;; Server Connection for kcl and ibcl

;;; Copyright (C) 1987, 1989 Massachussetts Institute of Technology 
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Massachussetts Institute of Technology provides this software "as is"
;;; without express or implied warranty.
;;;

;;; Adapted from code by Roman Budzianowski - Project Athena/MIT

;;; make-two-way-stream is probably not a reasonable thing to do.
;;; A close on a two way stream probably does not close the substreams.
;;; I presume an :io will not work (maybe because it uses 1 buffer?).
;;; There should be some fast io (writes and reads...).

;;; Compile this file with compile-file.
;;; Load it with (si:faslink "sockcl.o" "socket.o -lc")

(in-package :xlib)

;;; The cmpinclude.h file does not have this type definition from
;;; <kcldistribution>/h/object.h.  We include it here so the
;;; compile-file will work without figuring out where the distribution
;;; directory is located.
;;;
(CLINES "
enum smmode {			/*  stream mode  */
	smm_input,		/*  input  */
	smm_output,		/*  output  */
	smm_io,			/*  input-output  */
	smm_probe,		/*  probe  */
	smm_synonym,		/*  synonym  */
	smm_broadcast,		/*  broadcast  */
	smm_concatenated,	/*  concatenated  */
	smm_two_way,		/*  two way  */
	smm_echo,		/*  echo  */
	smm_string_input,	/*  string input  */
	smm_string_output,	/*  string output  */
	smm_user_defined        /*  for user defined */ 
};
")

#-akcl
(CLINES "
struct stream {
	short	t, m;
	FILE	*sm_fp;		/*  file pointer  */
	object	sm_object0;	/*  some object  */
	object	sm_object1;	/*  some object */
	int	sm_int0;	/*  some int  */
	int	sm_int1;	/*  some int  */
	short	sm_mode;	/*  stream mode  */
				/*  of enum smmode  */
};
")


;;;; Connect to the server.

;;; A lisp string is not a reasonable type for C, so copy the characters
;;; out and then call connect_to_server routine defined in socket.o

(CLINES "
int
konnect_to_server(host,display)
     object host;		/* host name */
     int    display;		/* display number */
{
   int fd;			/* file descriptor */
   int i;
   char hname[BUFSIZ];
   FILE *fout, *fin;

   if (host->st.st_fillp > BUFSIZ - 1)
     too_long_file_name(host);
   for (i = 0;  i < host->st.st_fillp;  i++)
     hname[i] = host->st.st_self[i];
   hname[i] = '\\0';            /* doubled backslash for lisp */

   fd = connect_to_server(hname,display);

   return(fd);
}
")

(defentry konnect-to-server (object int) (int "konnect_to_server"))


;;;; Make a one-way stream from a file descriptor.

(CLINES "
object
konnect_stream(host,fd,flag,elem)
     object host;		/* not really used */
     int fd;			/* file descriptor */
     int flag;			/* 0 input, 1 output */
     object elem;		/* 'string-char */
{
   struct stream *stream;
   char *mode;			/* file open mode */
   FILE *fp;			/* file pointer */
   enum smmode smm;		/* lisp mode (a short) */
   vs_mark;

   switch(flag){
    case 0:
      smm = smm_input;
      mode = \"r\";
      break;
    case 1:
      smm = smm_output;
      mode = \"w\";
      break;
    default:
      FEerror(\"konnect_stream : wrong mode\");
   }
   
   fp = fdopen(fd,mode);

   if (fp == NULL) {
     stream = Cnil;
     vs_push(stream);
   } else {
     stream = alloc_object(t_stream);
     stream->sm_mode = (short)smm;
     stream->sm_fp = fp;
     stream->sm_object0 = elem;
     stream->sm_object1 = host;
     stream->sm_int0 = stream->sm.sm_int1 = 0;
     vs_push(stream);
     setbuf(fp, alloc_contblock(BUFSIZ));
   }
   vs_reset;
   return(stream);
}
")

(defentry konnect-stream (object int int object) (object "konnect_stream"))


;;;; Open an X stream

(defun open-socket-stream (host display)
  (when (not (and (typep host    'string)	; sanity check the arguments
		  (typep display 'fixnum)))
    (error "Host ~s or display ~s are bad." host display))

  (let ((fd (konnect-to-server host display)))	; get a file discriptor
    (if (< fd 0)
	NIL
	(let ((stream-in  (konnect-stream host fd 0 'string-char))	; input
	      (stream-out (konnect-stream host fd 1 'string-char)))	; output
	  (if (or (null stream-in) (null stream-out))
	      (error "Could not make i/o streams for fd ~d." fd))
	  (make-two-way-stream stream-in stream-out))
	)))
