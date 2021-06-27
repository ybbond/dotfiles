import sublime
import sublime_plugin


class OpenContextUrlProxyCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        pos = self.view.text_to_window(self.view.sel()[0].begin())
        self.view.run_command('open_context_url', {
            "event": {
                "x": pos[0],
                "y": pos[1]
            }})
