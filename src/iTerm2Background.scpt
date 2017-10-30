tell application "iTerm2"
  repeat with w in windows
    set tablist to (the tabs of w)
    repeat with t in tablist
      set sessionlist to (the sessions of t)
      repeat with s in sessionlist
        tell s
	  set background image to "/tmp/bg.jpg"
	end tell
      end repeat
    end repeat
  end repeat
end tell