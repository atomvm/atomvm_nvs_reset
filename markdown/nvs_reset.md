# NVS Reset

The `atomvm_nvs_reset` Nif provides a mechanism to reset non-volatile storage (NVS) and reboot the device when a designated pin is held low (or optionally high) for a designated period of seconds, providing a kind of "paperclip" reset facility commonly found on routers, modems, and other electronic appliances.

This Nif does not provide any Erlang APIs.  Instead, it is implemented as a plugin module into the AtomVM virtual machine.

When installed, this Nif will instantiate an RTOS queue and start a lightweight RTOS task which monitors a designated GPIO pin.  If the pin is held low (or optionally high) for a designated period of seconds, NVS will be erased and (otpionally) the device will be rebooted.

This Nif is currently only supported on the ESP32 platform.

## Build Instructions

The `atomvm_nvs_reset` Nif is implemented as an AtomVM component, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this code, you must build an AtomVM binary image with this component compiled and linked into the image.

For general instructions about how to build AtomVM and include third-party components into an AtomVM image, see the [AtomVM Build Instructions](https://doc.atomvm.net/build-instructions.html).

Once the AtomVM image including this component has been built, you can flash the image to your ESP32 device.  For instructions about how to flash AtomVM images to your ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html).

### Configuration

The `atomvm_nvs_reset` Nif can be configured at build time via the IDF SDK `menuconfig` target.  For information about the IDF SDK build, see the IDF SDK [Getting Started](https://docs.espressif.com/projects/esp-idf/en/v4.4.2/esp32/get-started/index.html#) Guide.

The configuration menu for the `atomvm_nvs_reset` Nif can be found under the `Component config  --->`, `AtomVM NVS Reset Configuration  --->` menu.

The following table enumerates the configuration settings in this menu.

| Key | Type | Default | Description |
|------|------|---------|-------------|
| `ATOMVM_NVS_RESET_ENABLE` | `bool` | `y` | When enabled, a designated pin may be used to reset non-volatile storage when held during boot. |
| `ATOMVM_NVS_RESET_PIN` | `0..39` | `0` | GPIO pin used to reset non-volatile storage when held during boot. |
| `ATOMVM_NVS_RESET_HOLD_SECS` | `1..5` | `3` | Number of seconds to hold reset pin before non-volatile storage is reset. |
| `ATOMVM_NVS_RESET_INVERT_PIN` | `bool` | `n` | Invert the pin value so that the pin must be held high instead of low in order to reset non-volatile storage. |
| `ATOMVM_NVS_RESET_REBOOT` | `bool` | `y` | Reboot the device after resetting non-volatile storage. |
