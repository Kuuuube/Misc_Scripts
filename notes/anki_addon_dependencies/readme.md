# Anki Addon Dependencies

## Installing

Dependencies can be installed to a specific folder by using the `-t` flag. For example:

```
pip3 install requests -t ./lib
```

This will install the `requests` module to a folder named `lib`.

## Accessing

The module can now be imported in one of the following ways:

1. `from x import y`:

    ```
    from lib import requests
    ```

2. `import x.y`:

    ```
    import lib.requests
    ```

3. Adding lib folder to path:

    ```
    import sys
    # add lib folder to path to allow accessing modules there
    sys.path.append(sys.path[0] + "/lib")

    import requests
    ```

Run `setup_dependencies.sh` and `test.py` for a working example.
