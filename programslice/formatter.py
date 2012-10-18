

class LineFormatter(object):

    def __init__(self, slice_result, source):
        self.source = source
        self.slice_result = slice_result

    def __call__(self):
        return self.slice_result


class TextOutputFormatter(LineFormatter):

    def __call__(self):
        splitted = self.source.splitlines()
        filtered = [splitted[x - 1] for x in self.slice_result]
        return u'\n'.join(filtered)
