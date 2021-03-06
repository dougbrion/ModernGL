import moderngl.experimental as mgl

from example_window import Example, run_example


class EmptyWindow(Example):
    def __init__(self):
        self.ctx = mgl.create_context()

    def render(self):
        self.ctx.screen.viewport = self.wnd.viewport
        self.ctx.clear(0.2, 0.4, 0.7)


run_example(EmptyWindow)
