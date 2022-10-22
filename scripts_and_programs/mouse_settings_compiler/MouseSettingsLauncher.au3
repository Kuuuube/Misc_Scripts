#include <GUIConstantsEx.au3>
#include <RichEditConstants.au3>
#include <Constants.au3>
#include <MsgBoxConstants.au3>
#include <WindowsConstants.au3>
#include <GuiSlider.au3>
#include <GUIConstants.au3>

#AutoIt3Wrapper_Icon="polygoncursor.ico"

MouseSettingsLauncher ()

Func MouseSettingsLauncher ()



GUICreate ("MouseSettingsLauncher", 400, 235, @DesktopWidth / 2 - 200, @DesktopHeight / 2 - 150, -1, $WS_EX_ACCEPTFILES)
$speed = GUICtrlCreateSlider (100, 10, 250, 20)
GUICtrlSetData ($speed, 10)
GUICtrlSetLimit (-1, 20, 1)
$accel = GUICtrlCreateCheckbox ("", 106, 40, 25, 25)
$checkbox = "off"
$cursor = GUICtrlCreateInput ("", 106, 75, 238, 17)
GUICtrlSetState(-1, $GUI_DROPACCEPTED)
$ok = GUICtrlCreateButton ("OK", 105, 104, 50, 25)
GUICtrlCreateLabel ("Speed:", 10, 12)
GUICtrlCreateLabel ("Acceleration:", 10, 46)
GUICtrlCreateLabel ("Cursor Swap:", 10, 75)
GUICtrlCreateLabel ("Speed Slider: 1-20", 10, 135)
GUICtrlCreateLabel ("Default Speed: 10", 10, 155)
GUICtrlCreateLabel ("Accel: Checked = ON", 10, 175)
GUICtrlCreateLabel ("Cursor: Drop cursor file directly onto input box", 10, 195)
GUICtrlCreateLabel ("            or enter full cursor file path in input box", 10, 215)


GUISetState(@SW_SHOW)
While 1
		Switch GUIGetMsg()

				Case $GUI_EVENT_CLOSE
					ExitLoop
				Case $accel
                If _IsChecked($accel) Then
                    $checkbox = "on"
                Else
                    $checkbox = "off"
				EndIf
				Case $ok
				$speed = GUICtrlRead($speed)
				$cursor = GUICtrlRead($cursor)
				$run = "MouseSettingsCompiler.exe" & " " & "speed=" & $speed & " " & "accel=" & $checkbox & " " & "cursor=" & $cursor
					Run ($run, "")

					ExitLoop
			EndSwitch
		WEnd
EndFunc

Func _IsChecked($idControlID)
    Return BitAND(GUICtrlRead($idControlID), $GUI_CHECKED) = $GUI_CHECKED
EndFunc