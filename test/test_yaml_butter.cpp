#include "../yaml_butter.hpp"
#include <cassert>
#include <cmath>
#include <cstdint>    // for int64_t
#include <iostream>
#include <string>

int main() {
    // Load the YAML document
    YAML::Node root = YAML::ParseFile("test.yml");

    //
    // 1. Block‐style mapping with anchors, merge keys, multi‐line scalars
    //
    const auto& defaults = root["defaults"].AsMapping();
    assert(defaults.at("int_val").AsInt() == 123);
    assert(std::fabs(defaults.at("float_val").AsDouble() - 45.67) < 1e-9);
    assert(defaults.at("bool_true").AsBool());
    assert(!defaults.at("bool_false").AsBool());
    assert(defaults.at("plain_str").AsString() == "plainString");
    assert(defaults.at("quoted_str").AsString() == "a quoted string");

    std::cout << "defaults.literal_scalar:\n"
              << defaults.at("literal_scalar").AsString() << "\n";
    std::cout << "defaults.folded_scalar: "
              << defaults.at("folded_scalar").AsString() << "\n";

    const auto& block_map = root["block_map"].AsMapping();
    // inherited fields
    assert(block_map.at("int_val").AsInt() == defaults.at("int_val").AsInt());
    assert(std::fabs(block_map.at("float_val").AsDouble() - defaults.at("float_val").AsDouble()) < 1e-9);
    // extra key
    assert(block_map.at("extra_key").AsString() == "extraValue");
    std::cout << "block_map.extra_key: "
              << block_map.at("extra_key").AsString() << "\n";

    //
    // 2. Block‐style sequence with anchors, aliases, and nested blocks
    //
    const auto& block_seq = root["block_seq"].AsSequence();
    assert(block_seq.size() == 4);
    std::cout << "block_seq[0]: " << block_seq[0].AsString() << "\n";
    std::cout << "block_seq[2] (literal):\n"
              << block_seq[2].AsString() << "\n";

    const auto& copy_block_seq = root["copy_block_seq"].AsSequence();
    assert(copy_block_seq.size() == block_seq.size());
    std::cout << "copy_block_seq[1]: "
              << copy_block_seq[1].AsString() << "\n";

    //
    // 3. Flow‐style sequence and mapping at value positions
    //
    const auto& flow_seq = root["flow_seq"].AsSequence();
    assert(flow_seq[0].AsString() == "alpha");
    assert(flow_seq[1].AsString() == "beta");
    assert(flow_seq[2].AsString() == "gamma");
    assert(flow_seq[3].AsInt() == 4);
    assert(flow_seq[4].AsBool());
    assert(!flow_seq[5].AsBool());

    std::cout << "flow_seq:";
    for (size_t i = 0; i < flow_seq.size(); ++i) {
        std::cout << " ";
        if (auto v = flow_seq[i].TryAs<int64_t>(); v.has_value()) {
            std::cout << *v;
        }
        else if (auto b = flow_seq[i].TryAs<bool>(); b.has_value()) {
            std::cout << (*b ? "true" : "false");
        }
        else {
            std::cout << flow_seq[i].AsString();
        }
    }
    std::cout << "\n";

    const auto& flow_map = root["flow_map"].AsMapping();
    assert(flow_map.at("x").AsInt() == 1);
    assert(std::fabs(flow_map.at("y").AsDouble() - 2.5) < 1e-9);
    std::cout << "flow_map.z: " << flow_map.at("z").AsString() << "\n";
    const auto& nested_map = flow_map.at("nested_map").AsMapping();
    std::cout << "flow_map.nested_map.a: "
              << nested_map.at("a").AsString() << "\n";
    const auto& nested_seq = flow_map.at("nested_seq").AsSequence();
    std::cout << "flow_map.nested_seq[1]: "
              << nested_seq[1].AsString() << "\n";

    //
    // 4. Anchors & aliases in flow context
    //
    const auto& flow_map_anchor = root["flow_map_anchor"].AsMapping();
    const auto& copy_flow_map   = root["copy_flow_map"].AsMapping();
    assert(flow_map_anchor.at("k1").AsString() == "v1");
    assert(copy_flow_map.at("k2").AsString() == "v2");

    const auto& flow_seq_anchor = root["flow_seq_anchor"].AsSequence();
    const auto& copy_flow_seq   = root["copy_flow_seq"].AsSequence();
    assert(flow_seq_anchor[2].AsInt() == 30);
    assert(copy_flow_seq[0].AsInt() == 10);

    //
    // 5. Mixed block‐in‐flow and flow‐in‐block
    //
    const auto& mixed = root["mixed_values"].AsMapping();
    const auto& flow_inside = mixed.at("flow_inside").AsSequence();
    assert(flow_inside[1].AsString() == "y");
    const auto& map_inside = mixed.at("map_inside").AsMapping();
    assert(map_inside.at("inner1").AsInt() == 100);
    const auto& block_inside = mixed.at("block_inside").AsMapping();
    std::cout << "mixed.block_inside.subkey2: "
              << block_inside.at("subkey2").AsString() << "\n";

    //
    // 6. Sequence of flow mappings
    //
    const auto& seq_of_flow_maps = root["seq_of_flow_maps"].AsSequence();
    for (const auto& m : seq_of_flow_maps) {
        const auto& mp = m.AsMapping();
        std::cout << "seq_of_flow_maps entry:";
        for (const auto& [k,v] : mp) {
            std::cout << " " << k << "=";
            if (auto i = v.TryAs<int64_t>(); i.has_value()) {
                std::cout << *i;
            }
            else if (auto d = v.TryAs<double>(); d.has_value()) {
                std::cout << *d;
            }
            else if (auto b = v.TryAs<bool>(); b.has_value()) {
                std::cout << (*b ? "true" : "false");
            }
            else {
                std::cout << v.AsString();
            }
        }
        std::cout << "\n";
    }

    //
    // 7. Nested anchors and merge inside sequence
    //
    const auto& smb = root["seq_merge_block"].AsMapping();
    assert(smb.at("int_val").AsInt() == 123);
    const auto& items = smb.at("items").AsSequence();
    // first item is alias of flow_seq_anchor (a sequence)
    {
        const auto& alias_seq = items[0].AsSequence();
        assert(alias_seq[0].AsInt() == flow_seq_anchor[0].AsInt());
    }
    // second item is a flow sequence
    const auto& inner_flow = items[1].AsSequence();
    assert(inner_flow[2].AsInt() == 7);
    // third item is inner_map
    const auto& inner_map = items[2].AsMapping();
    assert(inner_map.at("im2").AsInt() == 200);
    const auto& merged_inner = smb.at("merged_inner").AsMapping();
    assert(merged_inner.at("im1").AsInt() == 100);

    //
    // 8. Root‐level flow mapping and sequence to test ParseBlock dispatch
    //
    const auto& root_flow_seq = root["root_flow_seq"].AsSequence();
    assert(root_flow_seq[0].AsInt() == 100);
    const auto& root_flow_map = root["root_flow_map"].AsMapping();
    std::cout << "root_flow_map.rm2: "
              << root_flow_map.at("rm2").AsString() << "\n";

    std::cout << "All YAML features tested successfully!\n";
    return 0;
}
