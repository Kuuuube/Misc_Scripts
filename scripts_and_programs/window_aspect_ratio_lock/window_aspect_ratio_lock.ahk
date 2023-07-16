global windowName := "Notepad"
global ratio := (16 / 9)

global widthOffset := 0
global heightOffset := 0

global sleepTime := 250 ; ms

SetTitleMatchMode 2

; Dont touch anything below this line

global prevWidth := 0
global prevHeight := 0
global coordChanging := 0

if WinExist(windowName) {
    WinGet, PID, PID, % "ahk_id " . owner:=WinExist()
    OnLocationChangeMonitor(owner, owned, PID)
    WinWaitClose % "ahk_id " . owner
}
ExitApp

OnLocationChangeMonitor(_hWinEventHook, _event, _hwnd) {
    static ox, oy, ow, ownerAhkId, ownedAhkId, hWinEventHook
    global rel_X, relY
    if !_hwnd {
        Return
    }
    if !hWinEventHook {
        hWinEventHook := SetWinEventHook("0x800B", "0x800B",0, RegisterCallback("OnLocationChangeMonitor"), _hwnd,0,0), ownerAhkId := _hWinEventHook,   ownedAhkId := _event,   OnExit(Func("UnhookWinEvent").Bind(hWinEventHook))
        WinGetPos,  _x,  _y,_w,_h, ahk_id %ownerAhkId%
        WinGetPos, _x1, _y1,,, ahk_id %ownedAhkId%
    }

    if WinExist(windowName) {
        WinGetPos, xPos, yPos, width, height
        if (width != prevWidth) {
            coordChanging = 1
        }
        else if (height != prevHeight)  {
            coordChanging = 2
        }
        sleep sleepTime
        WinGetPos, xPos, yPos, width, height
        if (width = prevWidth and coordChanging = 1) {
            WinMove,,,,, width - widthOffset, (width / ratio) - heightOffset
            coordChanging = 0
        }
        else if (height = prevHeight and coordChanging = 2) {
            WinMove,,,,, (height * ratio) - widthOffset, height - heightOffset
            coordChanging = 0
        }
        prevWidth = %width%
        prevHeight = %height%
    }
}

SetWinEventHook(_eventMin, _eventMax, _hmodWinEventProc, _lpfnWinEventProc, _idProcess, _idThread, _dwFlags) {
    DllCall("CoInitialize", "Uint", 0)
    return DllCall("SetWinEventHook","Uint",_eventMin,"Uint",_eventMax,"Ptr",_hmodWinEventProc,"Ptr",_lpfnWinEventProc,"Uint",_idProcess,"Uint",_idThread,"Uint",_dwFlags)
}

UnhookWinEvent(_hWinEventHook) {
    DllCall("UnhookWinEvent", "Ptr", _hWinEventHook)
    DllCall("CoUninitialize")
}
