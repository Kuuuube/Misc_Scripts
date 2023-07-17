global windowName := "Notepad"
global ratio := (16 / 9)

global widthOffset := 0
global heightOffset := 0

global sleepTime := 250 ; ms

SetTitleMatchMode 2

; Dont touch anything below this line

#SingleInstance

global hwnd := WinExist(windowName)
global hwndID := % "ahk_id " hwnd

WinGet, pid, PID, % hwndID

if hwnd and pid {
    OnLocationChangeMonitor(pid)
    WinWaitClose % hwndID
}
ExitApp

OnLocationChangeMonitor(pid) {
    static hWinEventHook, prevWidth, prevHeight, coordChanging

    if !hWinEventHook {
        DllCall("CoInitialize", "Uint", 0)
        DllCall("SetWinEventHook", "Uint", "0x800B", "Uint", "0x800B", "Ptr", 0, "Ptr", RegisterCallback("OnLocationChangeMonitor"), "Uint", pid, "Uint", 0, "Uint", 0)
    }

    WinGetPos,,, width, height, % hwndID
    if (width != prevWidth) {
        coordChanging := 1
    }
    else if (height != prevHeight)  {
        coordChanging := 2
    }

    sleep sleepTime

    WinGetPos,,, width, height, % hwndID
    if (width = prevWidth and coordChanging = 1) {
        WinMove, % hwndID,,,, width - widthOffset, (width / ratio) - heightOffset
        coordChanging := 0
    }
    else if (height = prevHeight and coordChanging = 2) {
        WinMove, % hwndID,,,, (height * ratio) - widthOffset, height - heightOffset
        coordChanging := 0
    }

    prevWidth := width
    prevHeight := height
}