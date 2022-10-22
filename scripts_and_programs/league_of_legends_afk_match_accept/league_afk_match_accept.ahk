#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%

Read_ini()

Read_ini()
{
    while 1
    {
        if FileExist("config.ini") != ""
        {
            IniRead, queue_button_offset_x_read, config.ini, league_afk_match_accept, queue_button_offset_x
            IniRead, queue_button_offset_y_read, config.ini, league_afk_match_accept, queue_button_offset_y
            IniRead, click_delay_read, config.ini, league_afk_match_accept, click_delay
            IniRead, window_name_read, config.ini, league_afk_match_accept, window_name

            global queue_button_offset_x = queue_button_offset_x_read + 0
            global queue_button_offset_y = queue_button_offset_y_read + 0
            global click_delay = click_delay_read + 0
            global window_name = window_name_read
            return
        }
        Else
        {
            config_file := FileOpen("config.ini", "w")
            config_file.WriteLine("[league_afk_match_accept]")
            config_file.WriteLine("queue_button_offset_x = 640")
            config_file.WriteLine("queue_button_offset_y = 560")
            config_file.WriteLine("click_delay = 500")
            config_file.Close()
        }
    }
}

!q::
    Pause

!w:: 
    While 1
    {
        if WinExist("League of Legends")
        {
            WinActivate
            WinGetPos, pos_x_top, pos_y_top, pos_x_bot, pos_y_bot

            queue_pos_x = %queue_button_offset_x% + %pos_x_top%
            queue_pos_y = %queue_button_offset_y% + %pos_y_top%
            MouseClick, Left, %queue_pos_x%, %queue_pos_y%
            Sleep, %click_delay%
        }
    }

^!w::
    ExitApp