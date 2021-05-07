# Unix folder

This folder ships with external resources required to run UltraGen. These will be typically dll used in some internal functions.

If interpreter presents some strange behavior or error, plese check this folder with the matching release.

## Contents:

1. libsagui-3.3.2

    - This dynamic library is used by BrookFramework which implements the Brook Server, one of the possible webservers available in UltraGen. It's a high performance application server, like Puma or Gunicorn. You should use this in production despite of the development server.
