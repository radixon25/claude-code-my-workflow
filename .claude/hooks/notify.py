#!/usr/bin/env python
"""
Cross-platform desktop notification hook.

On macOS: uses osascript.
On Windows: graceful no-op (optional win10toast support).
On Linux: uses notify-send if available.

Hook Event: Notification
"""

import json
import sys
import platform
import subprocess


def main():
    try:
        hook_input = json.load(sys.stdin)
    except (json.JSONDecodeError, IOError):
        hook_input = {}

    message = hook_input.get("message", "Claude needs attention")
    title = hook_input.get("title", "Claude Code")
    system = platform.system()

    if system == "Darwin":
        subprocess.run(
            ["osascript", "-e",
             f'display notification "{message}" with title "{title}"'],
            capture_output=True
        )
    elif system == "Windows":
        # Use PowerShell toast notification (built-in, no extra packages)
        try:
            ps_script = (
                f"[Windows.UI.Notifications.ToastNotificationManager, "
                f"Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null; "
                f"$template = [Windows.UI.Notifications.ToastNotificationManager]::"
                f"GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::"
                f"ToastText02); "
                f"$text = $template.GetElementsByTagName('text'); "
                f"$text.Item(0).AppendChild($template.CreateTextNode('{title}')) | Out-Null; "
                f"$text.Item(1).AppendChild($template.CreateTextNode('{message}')) | Out-Null; "
                f"$toast = [Windows.UI.Notifications.ToastNotification]::new($template); "
                f"[Windows.UI.Notifications.ToastNotificationManager]::"
                f"CreateToastNotifier('Claude Code').Show($toast)"
            )
            subprocess.run(
                ["powershell", "-Command", ps_script],
                capture_output=True, timeout=5
            )
        except Exception:
            pass  # Graceful no-op if notification fails
    elif system == "Linux":
        try:
            subprocess.run(
                ["notify-send", title, message],
                capture_output=True, timeout=5
            )
        except FileNotFoundError:
            pass  # notify-send not installed

    sys.exit(0)


if __name__ == "__main__":
    main()
