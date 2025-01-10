# Anki Addon Dependencies

Dependencies can be installed to a specific folder by using the `-t` flag. For example:

```
pip3 install requests -t ./lib
```

This will install the `requests` module to a folder named `lib`. The module can now be imported in one of the following ways:

```
from lib import requests
```

```
import lib.requests
```

Run `setup_dependencies.sh` and `test.py` for a working example.
