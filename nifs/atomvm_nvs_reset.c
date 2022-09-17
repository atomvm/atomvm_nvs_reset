//
// Copyright (c) dushin.net
// All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <driver/gpio.h>
#include <freertos/FreeRTOS.h>
#include <freertos/queue.h>
#include <esp_log.h>
#include <nvs.h>
#include <nvs_flash.h>
#include <sdkconfig.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/time.h>
#include <utils.h>

#include "atomvm_nvs_reset.h"
#include "esp32_sys.h"

//#define ENABLE_TRACE
#include "trace.h"

#define TAG "atomvm_nvs_reset"
#define BUTTON_PRESS_QUEUE_SIZE 16
#define BUTTON_PRESS_TASK_STACK 5000
#define BUTTON_PRESS_TASK_SLEEP_MS 50

static xQueueHandle button_press_queue;

static uint64_t current_ms(void)
{
    struct timeval tv_now;
    gettimeofday(&tv_now, NULL);
    uint64_t time_ms = (uint64_t) tv_now.tv_sec * 1000L + (uint64_t) tv_now.tv_usec / 1000L;
    return time_ms;
}

static void button_press_handler(void *arg)
{
    int msg = 1;
    BaseType_t res = xQueueSendToBackFromISR(button_press_queue, &msg, 0);
    if (res != pdPASS) {
        ESP_LOGW(TAG, "Unable to push message to button_press_queue!");
    }
}

static void monitor_button_task(void *args)
{
    bool invert_pin =
#if defined(CONFIG_ATOMVM_NVS_RESET_INVERT_PIN)
        true;
#else
        false;
#endif
    bool queue_created = false;
    button_press_queue = xQueueCreate(BUTTON_PRESS_QUEUE_SIZE, sizeof(int));
    if (IS_NULL_PTR(button_press_queue)) {
        ESP_LOGE(TAG, "Failed to initialize button_press_queue.");
        goto halt;
    }
    queue_created = true;

    gpio_num_t pin = CONFIG_ATOMVM_NVS_RESET_PIN;
    esp_err_t err;

    err = gpio_set_direction(pin, GPIO_MODE_INPUT);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to set pin %i to input mode.  err=%i", pin, err);
        goto halt;
    }
    gpio_pull_mode_t pull_mode = GPIO_PULLUP_ONLY;
    if (invert_pin) {
        pull_mode = GPIO_PULLDOWN_ONLY;
    }
    err = gpio_set_pull_mode(pin, pull_mode);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to set pin %i to internal pullup.  err=%i", pin, err);
        goto halt;
    }
    err = gpio_install_isr_service(0);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to install ISR service.  err=%i", err);
        goto halt;
    }
    err = gpio_set_intr_type(pin, GPIO_INTR_ANYEDGE);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to set rising edge interrupt on pin %i.  err=%i", pin, err);
        goto halt;
    }

    bool interrupt_added = false;
    err = gpio_isr_handler_add(pin, button_press_handler, NULL);
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Failed to set ISR handler for pin %i.  err=%i", pin, err);
        goto halt;
    }
    interrupt_added = true;

    ESP_LOGI(TAG, "monitor_button_task is running.");

    uint64_t start_ms = current_ms();
    for (;;) {
        uint64_t elapsed_ms = current_ms() - start_ms;
        int msg;
        BaseType_t res = xQueueReceive(button_press_queue, &msg, 0);
        if (res == pdPASS) {
            // received a message
            int level = gpio_get_level(pin) ^ invert_pin;
            if (!level) {
                // button pressed
                start_ms = current_ms();
                ESP_LOGI(TAG, "Pin %i pressed.", pin);
            } else {
                // button released
                if (elapsed_ms > CONFIG_ATOMVM_NVS_RESET_HOLD_SECS * 1000) {
                    ESP_LOGI(TAG, "Pin %i released after %llu ms.", pin, elapsed_ms);
                    err = nvs_flash_erase();
                    if (err != ESP_OK) {
                        ESP_LOGW(TAG, "Failed to erase flash.  err=%i", err);
                    } else {
                        ESP_LOGI(TAG, "Erased NVS flash!");
                        if (CONFIG_ATOMVM_NVS_RESET_REBOOT) {
                            ESP_LOGI(TAG, "Restarting device ...");
                            esp_restart();
                        }
                    }
                } else {
                    ESP_LOGI(TAG, "Pin %i released too early!  elapsed_ms=%llu", pin, elapsed_ms);
                }
            }
        }
        vTaskDelay(BUTTON_PRESS_TASK_SLEEP_MS / portTICK_PERIOD_MS);
    }

halt:

    // remove the interrupt if it was added
    if (interrupt_added) {
        err = gpio_isr_handler_remove(pin);
        if (err != ESP_OK) {
            ESP_LOGW(TAG, "Failed to remove ISR for pin %i.  err=%i", pin, err);
        }
    }

    // reset the pin
    err = gpio_reset_pin(pin);
    if (err != ESP_OK) {
        ESP_LOGW(TAG, "Failed to reset pin %i.  err=%i", pin, err);
    }

    // destroy the queue if it was created
    if (queue_created) {
        vQueueDelete(button_press_queue);
    }

    ESP_LOGW(TAG, "An error was detected in monitor_button_task.  This task will halt!");
    vTaskDelete(NULL);
}

//
// Nif entrypoints
//

void atomvm_nvs_reset_init(GlobalContext *global)
{
    BaseType_t res = xTaskCreate(monitor_button_task, "monitor_button_task", BUTTON_PRESS_TASK_STACK, NULL, 1, NULL);
    if (res != pdPASS) {
        ESP_LOGE(TAG, "Failed to initialize monitor_button_task.  Error: %i", res);
    }
}

const struct Nif *atomvm_nvs_reset_get_nif(const char *nifname)
{
    // this Nif does not currently support nif functions
    return NULL;
}

#include <sdkconfig.h>
#ifdef CONFIG_ATOMVM_NVS_RESET_ENABLE
REGISTER_NIF_COLLECTION(atomvm_nvs_reset, atomvm_nvs_reset_init, atomvm_nvs_reset_get_nif)
#endif
