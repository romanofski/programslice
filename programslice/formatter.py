

class LineFormatter(object):

    def __init__(self, slice_result, source):
        self.source = source
        self.slice_result = slice_result

    def __call__(self):
        return self.slice_result
