#include "../yaml_butter.hpp"
#include <cassert>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <string>

/// Pretty-print a node's value and tag (for scalar types).
void PrintScalarWithTag(const YAML::Node& node, const std::string& label) {
    std::cout << label << ": ";
    if (!node.Tag().empty())
        std::cout << "[" << node.Tag() << "] ";
    if (node.IsNull()) std::cout << "(null)";
    else if (node.Type() == YAML::Node::Kind::Bool) std::cout << (node.AsBool() ? "true" : "false");
    else if (node.Type() == YAML::Node::Kind::Int) std::cout << node.AsInt();
    else if (node.Type() == YAML::Node::Kind::Double) std::cout << node.AsDouble();
    else if (node.Type() == YAML::Node::Kind::String) std::cout << node.AsString();
    else std::cout << "(non-scalar)";
    std::cout << "\n";
}

/// Pretty-print a sequence, showing tags on each element.
void PrintSequenceWithTags(const YAML::Node& seq, const std::string& label) {
    std::cout << label << ":\n";
    size_t idx = 0;
    for (const auto& elt : seq.AsSequence()) {
        std::cout << "  [" << idx++ << "] ";
        PrintScalarWithTag(elt, "");
    }
}

/// Pretty-print a mapping, showing tags on each value.
void PrintMappingWithTags(const YAML::Node& map, const std::string& label) {
    std::cout << label << ":\n";
    for (const auto& kv : map.AsMapping()) {
        std::cout << "  " << kv.first << ": ";
        PrintScalarWithTag(kv.second, "");
    }
}

int main() {
    YAML::Node root = YAML::ParseFile("test.yml");

    // Block-style mapping with tags
    const auto& defaults = root["defaults"].AsMapping();
    PrintScalarWithTag(defaults.at("int_val"), "defaults.int_val");
    PrintScalarWithTag(defaults.at("float_val"), "defaults.float_val");
    PrintScalarWithTag(defaults.at("bool_true"), "defaults.bool_true");
    PrintScalarWithTag(defaults.at("bool_false"), "defaults.bool_false");
    PrintScalarWithTag(defaults.at("plain_str"), "defaults.plain_str");
    PrintScalarWithTag(defaults.at("quoted_str"), "defaults.quoted_str");
    PrintScalarWithTag(defaults.at("literal_scalar"), "defaults.literal_scalar");
    PrintScalarWithTag(defaults.at("folded_scalar"), "defaults.folded_scalar");

    // Mapping with merge key and custom tag
    const auto& block_map = root["block_map"].AsMapping();
    PrintScalarWithTag(block_map.at("extra_key"), "block_map.extra_key (should have !CustomType tag)");

    // Block sequence with tags
    PrintSequenceWithTags(root["block_seq"], "block_seq");
    PrintSequenceWithTags(root["copy_block_seq"], "copy_block_seq (alias)");

    // Flow sequence with tags
    PrintSequenceWithTags(root["flow_seq"], "flow_seq");

    // Flow mapping with nested tags
    const auto& flow_map = root["flow_map"].AsMapping();
    PrintScalarWithTag(flow_map.at("x"), "flow_map.x");
    PrintScalarWithTag(flow_map.at("y"), "flow_map.y");
    PrintScalarWithTag(flow_map.at("z"), "flow_map.z");
    PrintMappingWithTags(flow_map.at("nested_map"), "flow_map.nested_map");
    PrintSequenceWithTags(flow_map.at("nested_seq"), "flow_map.nested_seq");

    // Anchors and aliases in flow context
    PrintMappingWithTags(root["flow_map_anchor"], "flow_map_anchor");
    PrintMappingWithTags(root["copy_flow_map"], "copy_flow_map (alias)");
    PrintSequenceWithTags(root["flow_seq_anchor"], "flow_seq_anchor");
    PrintSequenceWithTags(root["copy_flow_seq"], "copy_flow_seq (alias)");

    // Mixed block-in-flow and flow-in-block
    const auto& mixed = root["mixed_values"].AsMapping();
    PrintSequenceWithTags(mixed.at("flow_inside"), "mixed.flow_inside");
    PrintMappingWithTags(mixed.at("map_inside"), "mixed.map_inside");
    PrintMappingWithTags(mixed.at("block_inside"), "mixed.block_inside");

    // Sequence of flow mappings, including a custom tag
    const auto& seq_of_flow_maps = root["seq_of_flow_maps"].AsSequence();
    for (size_t i = 0; i < seq_of_flow_maps.size(); ++i) {
        std::cout << "seq_of_flow_maps[" << i << "]:\n";
        for (const auto& kv : seq_of_flow_maps[i].AsMapping()) {
            PrintScalarWithTag(kv.second, std::string("  ") + kv.first);
        }
    }

    // Nested anchors and merge inside sequence
    const auto& smb = root["seq_merge_block"].AsMapping();
    PrintScalarWithTag(smb.at("int_val"), "seq_merge_block.int_val");
    const auto& items = smb.at("items").AsSequence();
    // Just print tags for all sequence elements
    for (size_t i = 0; i < items.size(); ++i) {
        if (items[i].IsSequence()) PrintSequenceWithTags(items[i], "seq_merge_block.items[" + std::to_string(i) + "]");
        else if (items[i].IsMapping()) PrintMappingWithTags(items[i], "seq_merge_block.items[" + std::to_string(i) + "]");
        else PrintScalarWithTag(items[i], "seq_merge_block.items[" + std::to_string(i) + "]");
    }
    PrintMappingWithTags(smb.at("merged_inner"), "seq_merge_block.merged_inner");

    // Root-level flow sequence and mapping
    PrintSequenceWithTags(root["root_flow_seq"], "root_flow_seq");
    PrintMappingWithTags(root["root_flow_map"], "root_flow_map");

    std::cout << "\nAll YAML features and tags tested successfully!\n";
    return 0;
}
