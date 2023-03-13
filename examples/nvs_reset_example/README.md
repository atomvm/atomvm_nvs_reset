# AtomVM NVS Reset Example Program

The `nvs_reset` program illustrates use of the (Erlang-based) NVS Reset service.

To run this example program, you should connect a press button between GND and IO0 (or a pin of your choice) on your ESP32 device:

                +-------+
                |   GND o-------+
                |       |       |
                |       |       = press button
                |       |       |
                |   IO0 o-------+
                +-------+
                  ESP32

> Note that many development boards (e.g., the Espressif DevKitC) configure the "boot" button to IO0.

Building and flashing the `nvs_reset` program requires installation of the [`rebar3`](https://www.rebar3.org) Erlang build tool.

Build the example program and flash to your device:

    shell$ rebar3 packbeam -p
    shell$ rebar3 esp32_flash -p /dev/ttyUSB0

> Note.  This build step makes use of the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin).  See the `README.md` for information about parameters for setting the serial port and baud rate for your platform.

Attach to the console using `minicom` or equivalent:

    shell$ minicom -D /dev/ttyUBS0
    ...
    I (0) cpu_start: Starting scheduler on APP CPU.

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    I (859) AtomVM: Starting AtomVM revision 0.6.0-dev+git.dc1e8401
    I (869) AtomVM: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    I (899) AtomVM: Found startup beam nvs_reset_example.beam
    I (899) AtomVM: Loaded BEAM partition lib.avm at address 0x1d0000 (size=262144 bytes)
    I (909) AtomVM: Starting nvs_reset_example.beam...
    ---
    NVS test value at startup: undefined
    Set NVS test value to: <<"this is a test">>
    NVS Reset started on pin 0.

Note that an NVS value us set to `<<"this is a test">>` after the program starts.

To verify, restart the device, and you should see the following output on the console after restart:

    NVS test value at startup: <<"this is a test">>
    Set NVS test value to: <<"this is a test">>
    NVS Reset started on pin 0.

Press the reset button momentarily (for less than 5 seconds).  You should see the following output on the console:

    Button 0 pressed.
    Button released too early (must be held for 5 secs but only held for 437 ms).

Now press and hold the reset button for more than 5 seconds (but less than 15).  The device should reset NVS storage and reboot.  You should see the following output on the console:

    Erasing NVS ...
    Warning: Reformatted NVS partition!
    Restarting ...ets Jun  8 2016 00:22:57

The ESP32 will restart, and you should see the following output on the console after restart:

    NVS test value at startup: undefined
    Set NVS test value to: <<"this is a test">>
    NVS Reset started on pin 0.
