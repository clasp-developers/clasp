if not exist %1 ( echo file_copy failure: %1 not found && goto exit)
if exist %2     ( fc %1 %2 > nul && if not %errorlevel 1 goto exit )
echo copying %1 to %2 && copy %1 %2
:exit