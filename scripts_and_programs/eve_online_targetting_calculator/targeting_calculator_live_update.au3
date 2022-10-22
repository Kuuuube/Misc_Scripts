#include <GUIConstantsEx.au3>
#include <RichEditConstants.au3>
#include <Constants.au3>
#include <MsgBoxConstants.au3>
#include <WindowsConstants.au3>
#include <GUIConstants.au3>

EvE_Targeting_Calculator ()

Func EvE_Targeting_Calculator ()



GUICreate ("EvE Targeting Calculator", 400, 109, @DesktopWidth / 2 - 200, @DesktopHeight / 2 - 150)
$sigradinput = GUICtrlCreateInput ("", 106, 12, 238, 17)
$scanresinput = GUICtrlCreateInput ("", 106, 32, 238, 17)
$speed = GUICtrlCreateInput ("", 106, 52, 238, 17)
$ok = GUICtrlCreateButton ("OK", 105, 72, 50, 25)
GUICtrlCreateLabel ("Signature Radius", 10, 12)
GUICtrlCreateLabel ("Scan Resolution", 10, 32)
GUICtrlCreateLabel ("Targeting Speed", 10, 52)


GUISetState(@SW_SHOW)
	While 1
			$sigrad = GUICtrlRead($sigradinput)
			$scanres = GUICtrlRead($scanresinput)
			$arcsinh = Log($sigrad+(Sqrt($sigrad^2+1))/Log(2.71828182845904523536028747135266249775724709369995))
			$formula = (40000/$scanres)/($arcsinh^2)
			GUICtrlSetData ($speed, $formula)
		Switch GUIGetMsg()
			Case $GUI_EVENT_CLOSE
				ExitLoop
		EndSwitch
	WEnd
EndFunc