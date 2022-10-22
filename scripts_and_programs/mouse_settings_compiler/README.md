# Mouse Settings Compiler

Change mouse settings on windows without admin privileges and without control panel/settings access.

## Usage

- Run `MouseSettingsLauncher.exe` or `MouseSettingsLauncher.au3` to use the GUI.

    OR

- Run `MouseSettingsCompiler.exe` to use CLI only. It accepts the following command line args:

    speed={1-20}

    accel={on|off|toggle}

    cursor={path to ani/cur file|Default}

    For example: 

    ```
    MouseSettingsCompiler.exe speed=10 accel=off cursor=cursor.cur
    ```

## Dependencies

Install [AutoIt](https://www.autoitscript.com) and run the scripts using `AutoIt3_x64.exe` if you want to run from the .au3 file. This step is not required when using the .exe file.

## Notes

- I did not code `MouseSettingsCompiler.exe`, only `MouseSettingsLauncher.au3/MouseSettingsLauncher.exe`. 

    I got `MouseSettingsCompiler.exe` from some old forum post that I will probably never find again and unfortunately I don't have the source code.

    It is likely easy to decompile if you want to know how it works.

- I cannot guarantee this will always function since it is seemingly an exploit even though it has been around for over 12 years at the time of writing.