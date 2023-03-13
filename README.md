# AtomVM NVS Reset Driver

The AtomVM NVS Reset service

The AtomVM Nif can be used to reset non-volatile storage (NVS) on an ESP32 device by holding a designated pin for a designated period of seconds.

This driver is included as an add-on to the AtomVM base image.  In order to use this driver, you must be able to build the AtomVM virtual machine, which in turn requires installation of the Espressif IDF SDK and tool chain.

This Nif is only supported on the ESP32 platform.

Documentation for this component can be found in the following sections:

* [User Guide](markdown/nvs_reset.md)
