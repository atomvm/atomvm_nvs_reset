# NVS Reset

The NVS Reset service provides a mechanism to reset non-volatile storage (NVS) and reboot the device when a designated pin is held low (or optionally high) for a designated period of seconds, providing a kind of "paperclip" reset facility commonly found on routers, modems, and other electronic appliances.

This repository contains two implementations of the NVS Reset service, a Nif-based implementation, and a pure Erlang based implementation.

The tradeoffs between the two implementations are listed in the following table:

| Implementation | Advantages | Disadvantages |
|----------------|------------|---------------|
| Erlang-based | Easy to deploy and runs on plain AtomVM | Required to be included in all applications if re-flashed |
| Nif-based | Built-into the VM, so it cannot be deleted | Requires a custom build of AtomVM |

The Nif-based services was built with the intention of supporting soft OTA updates, whereby you may want to guarantee that the ability to reset the device is always available.  Otherwise, if you were to perform an OTA update, and for some reason the application you upload has a bug or failed tos tart, you may not be able to reset the NVS storage using the reset pin to get it back into a "factory" setting mode.  The Nif-based approach is therefore more appropriate for fail-safe OTA update scenarios.

Some applications may only need a simple reset capability in their existing applications.  In that case, the pure Erlang-based approach may be more appropriate and is far simpler to deploy, as it does not require a custom build of AtomVM.

## Pure Erlang implementation

The pure Erlang-based implementation is implemented as an Erlang process (`atomvm_nvs_reset`), which when started will monitor a specified pin.  If the pin is held for a specified number of seconds or more (up to a specified threshold), the NVS storage will be reset, and (if configured), the device will be restarted.

You can use this pin as a return to factory settings "paperclip" pin.

To start the `atomvm_nvs_reset` service, use the `start_link/1` function, supplying a valid configuration:

    %% Erlang
    Config = #{
        reset_pin => 4
    },
    {ok, _Pid} = atomvm_nvs_reset:start_link(Config),

The `Config` parameter is a map, containing (optionally) the following values:

| Key | Value Type | Default | Description |
|-----|------------|---------|-------------|
| `reset_pin` | `non_neg_integer()` | `0` | GPIO pin to monitor |
| `reset_invert_pin` | `boolean()` | `false` | If `false`, pin is pulled high and triggers when pin is low; if `true`, pin is pulled low and triggers when pin is high. |
| `reset_hold_secs` | `non_neg_integer()` | `5` | Minimum number of seconds the `reset_pin` must be held to trigger and NVS reset |
| `reset_hold_timeout_secs` | `non_neg_integer()` | `15` | If the `reset_pin` is held for more than this value, then the NVS reset will time out. |
| `restart_on_reset` | `boolean()` | `true` | If `true`, then the device will be restarted after the NVS has been reset. |


## Nif-based implementation

The `atomvm_nvs_reset` Nif provides a mechanism to reset non-volatile storage (NVS) and reboot the device when a designated pin is held low (or optionally high) for a designated period of seconds, providing a kind of "paperclip" reset facility commonly found on routers, modems, and other electronic appliances.

The reset pin may also be configured to only reset the boot partition after a specified number of button clicks (by default, 3) within a designated time window (by default, 1, second).  Using this feature, you can preserve all application-level configuration and only reset which partition the ESP32 device should boot from.

This Nif does not provide any Erlang APIs.  Instead, it is implemented as a plugin module into the AtomVM virtual machine.

When installed, this Nif will instantiate an RTOS queue and start a lightweight RTOS task which monitors a designated GPIO pin.  If the pin is held low (or optionally high) for a designated period of seconds, NVS will be erased and (otpionally) the device will be rebooted.

This Nif is currently only supported on the ESP32 platform.

### Build Instructions

The `atomvm_nvs_reset` Nif is implemented as an AtomVM component, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this code, you must build an AtomVM binary image with this component compiled and linked into the image.

For general instructions about how to build AtomVM and include third-party components into an AtomVM image, see the [AtomVM Build Instructions](https://doc.atomvm.net/build-instructions.html).

Once the AtomVM image including this component has been built, you can flash the image to your ESP32 device.  For instructions about how to flash AtomVM images to your ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html).

#### Configuration

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
| `ATOMVM_NVS_RESET_BOOT_PARTITION_CLICKS` | `int` | `3` | The number of clicks that must occur within `NVS_RESET_BOOT_PARTITION_CLICK_WINDOW_SECS` to reset to the boot partition only. |
| `ATOMVM_NVS_RESET_BOOT_PARTITION_CLICK_WINDOW_SECS` | `int` | `1` | Time interval (secs) in which button clicks should occur. |
