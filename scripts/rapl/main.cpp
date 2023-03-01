#include <string.h>

#include <cassert>
#include <iostream>

#include <papi.h>

int getPAPIComponentId(const std::string& name) {
    for (int i = 0; i < PAPI_num_components(); ++i) {
        const auto* info = PAPI_get_component_info(i);
        // if (info != nullptr && !info->disabled && name == info->name) {
        //     return i;
        // }

        // if (info != nullptr && name == info->name) {
            
            if (info->disabled) {
                std::cout << info->name << ": " << info->disabled_reason << std::endl;
            } else {
                std::cout << info->name << ": " << "not disabled" << std::endl;
            }
        // }
    }

    return -1;
}

int main() {
    const auto version = PAPI_library_init(PAPI_VER_CURRENT);
    assert(version == PAPI_VER_CURRENT);

    const auto cid = getPAPIComponentId("rapl");
    assert(cid >= 0);
    
    std::cout << "Hello, World!" << std::endl;
    std::cout << PAPI_ESYS << std::endl;
}
