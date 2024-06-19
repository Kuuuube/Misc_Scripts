cd "$(dirname "$0")"
nohup bash -c "python ~/free_badge/app.py >> console.log 2>> console_error.log" &
sleep 3
ps aux | grep free_badge | grep -v grep