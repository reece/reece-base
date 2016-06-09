try:
    import colorama

    def highlight(s, substr):
        return s.replace(substr,
                        colorama.ansi.Style.BRIGHT + colorama.ansi.Fore.YELLOW
                        + substr + colorama.ansi.Style.RESET_ALL)

except ImportError:
    logging.warn("Couldn't import colorama")
