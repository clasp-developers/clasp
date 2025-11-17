(in-package #:mp)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(;; locks
          with-lock with-rwlock
          ;; processes
          abort-process
          ;; atomic operations
          atomic fence cas get-atomic-expansion define-atomic-expander
          not-atomic not-atomic-place
          atomic-update atomic-update-explicit
          atomic-incf atomic-decf atomic-incf-explicit atomic-decf-explicit
          atomic-push atomic-push-explicit atomic-pop atomic-pop-explicit
          atomic-pushnew atomic-pushnew-explicit
          ;; interrupts
          interrupt service-interrupt interrupt-process
          process-kill process-cancel process-suspend
          simple-interrupt simple-interactive-interrupt
          cancellation-interrupt suspension-interrupt
          call-interrupt call-interrupt-function
          signal-pending-interrupts raise
          without-interrupts with-interrupts with-local-interrupts
          with-restored-interrupts allow-with-interrupts interruptiblep
          ))
) ; eval-when
