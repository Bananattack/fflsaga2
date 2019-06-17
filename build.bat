@echo off
setlocal
echo.
echo Compiling...
rgbasm -o main.o main.asm^
    && rgblink -O ffl2-orig.gb -n ffl2.sym -o ffl2.gb main.o^
    && rgbfix -v ffl2.gb
if %errorlevel% neq 0 goto :failed_code
echo.
echo Success!
goto done


:failed_assets
echo.
echo **************************
echo Failure! (assets)
echo **************************
echo.
goto done

:failed_code
echo.
echo **************************
echo Failure! (code)
echo **************************
echo.
goto done

:done
pause