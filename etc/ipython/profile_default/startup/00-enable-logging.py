import logging
import os

logging.basicConfig(level=logging.getLevelName(os.environ.get('IPYTHON_LOG_LEVEL','INFO')))
