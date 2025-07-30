#include "yaml_butter.hpp"
#include <iostream>

int main() {
    YAML::Node root = YAML::ParseFile("test.yml");

    // server1 inherits from defaults
    const auto& server1 = root["server1"].AsMapping();
    std::cout << "server1.host: " << server1.at("host").AsString() << "\n";
    std::cout << "server1.retries (from defaults): " << server1.at("retries").AsInt() << "\n";
    std::cout << "server1.timeout (from defaults): " << server1.at("timeout").AsInt() << "\n";
    std::cout << "server1.greeting (multi-line):\n" << server1.at("greeting").AsString() << "\n";
    std::cout << "server1.note (folded): " << server1.at("note").AsString() << "\n";

    const auto& list = root["list"].AsSequence();
    std::cout << "Original list:\n";
    for (const auto& item : list)
        std::cout << "  - " << item.AsString() << "\n";

    const auto& copied_list = root["copy_of_list"].AsSequence();
    std::cout << "Aliased (copied) list:\n";
    for (const auto& item : copied_list)
        std::cout << "  - " << item.AsString() << "\n";
}
