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

#ifndef __ATOMVM_NVS_RESET_H__
#define __ATOMVM_NVS_RESET_H__

#include <globalcontext.h>
#include <nifs.h>

void atomvm_nvs_reset_init(GlobalContext *global);
const struct Nif *atomvm_nvs_reset_get_nif(const char *nifname);

#endif
