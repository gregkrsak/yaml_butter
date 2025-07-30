/**
 * @file yaml_butter.hpp
 * @brief Single-header YAML parser for C++23 with anchors, aliases, merge keys, and multi-line scalars.
 *
 * This header-only YAML parser supports:
 *   - Block-style Mappings (dictionary/object), Sequences (array/list), and Scalars (string, int, double, bool, null)
 *   - Indentation-based parsing (block style YAML)
 *   - Multi-line scalars (literal '|' and folded '>')
 *   - Anchors and aliases (&anchor, *alias) for mappings, sequences, and scalars
 *   - Merge keys (<<: *anchor) for mappings
 *
 * Usage Example:
 * @code
 * #include "yaml_butter.hpp"
 * YAML::Node root = YAML::ParseFile("test.yml");
 * std::string host = root["server1"]["host"].AsString();
 * @endcode
 *
 * @author Greg M. Krsak <greg.krsak@gmail.com>
 * @note Vibe coded with ChatGPT models GPT-4.1 [Base model] as primary, with backup from GPT-4 [Omni 4 Mini High model]
 * @date July 30, 2025
 * @copyright MIT License
 */

#ifndef YAML_BUTTER_HPP
#define YAML_BUTTER_HPP

#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <map>
#include <unordered_map>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <cctype>
#include <optional>
#include <charconv>
#include <algorithm>

/**
 * @namespace YAML
 * @brief Namespace containing all YAML parser types and functions.
 */
namespace YAML {

/**
 * @class Node
 * @brief Represents a YAML node (Null, Bool, Int, Double, String, Sequence, or Mapping).
 *
 * A YAML node is a tagged variant and can hold:
 *   - Null
 *   - Boolean
 *   - Integer (int64_t)
 *   - Double
 *   - String
 *   - Sequence (std::vector<Node>)
 *   - Mapping (std::map<std::string, Node>)
 *
 * Provides type-safe accessors, introspection, and operator overloads for sequence/mapping access.
 */
class Node {
public:
    /**
     * @enum Kind
     * @brief Enumeration of all YAML node types.
     */
    enum class Kind { Null, Bool, Int, Double, String, Sequence, Mapping };

    /**
     * @typedef Value
     * @brief Variant type for storing any supported node value.
     */
    using Value = std::variant<
        std::monostate,            /**< Null */
        bool,                      /**< Boolean */
        int64_t,                   /**< Integer */
        double,                    /**< Double */
        std::string,               /**< String */
        std::vector<Node>,         /**< Sequence */
        std::map<std::string, Node>/**< Mapping */
    >;

    /**
     * @brief Default constructor. Initializes as Null.
     */
    Node() noexcept : kind_(Kind::Null), value_(std::monostate{}) {}

    /**
     * @brief Construct from a boolean value.
     * @param b Boolean value.
     */
    Node(bool b) noexcept : kind_(Kind::Bool), value_(b) {}

    /**
     * @brief Construct from an integer value.
     * @param i Integer value.
     */
    Node(int64_t i) noexcept : kind_(Kind::Int), value_(i) {}

    /**
     * @brief Construct from a double value.
     * @param d Double value.
     */
    Node(double d) noexcept : kind_(Kind::Double), value_(d) {}

    /**
     * @brief Construct from a std::string value.
     * @param s String value.
     */
    Node(std::string s) noexcept : kind_(Kind::String), value_(std::move(s)) {}

    /**
     * @brief Construct from a C-string value.
     * @param s Null-terminated C-string.
     */
    Node(const char* s) : kind_(Kind::String), value_(std::string(s)) {}

    /**
     * @brief Construct from a vector (YAML sequence).
     * @param seq Sequence of YAML nodes.
     */
    Node(std::vector<Node> seq) noexcept : kind_(Kind::Sequence), value_(std::move(seq)) {}

    /**
     * @brief Construct from a map (YAML mapping).
     * @param map Mapping of strings to YAML nodes.
     */
    Node(std::map<std::string, Node> map) noexcept : kind_(Kind::Mapping), value_(std::move(map)) {}

    /**
     * @brief Returns the type of this node.
     * @return Kind of the node.
     */
    Kind Type() const noexcept { return kind_; }

    /**
     * @brief Returns true if the node is Null.
     * @return True if Null, false otherwise.
     */
    bool IsNull() const noexcept { return kind_ == Kind::Null; }

    /**
     * @brief Returns true if the node is a scalar (Bool, Int, Double, or String).
     * @return True if scalar, false otherwise.
     */
    bool IsScalar() const noexcept {
        return kind_ == Kind::Bool
            || kind_ == Kind::Int
            || kind_ == Kind::Double
            || kind_ == Kind::String;
    }

    /**
     * @brief Returns true if the node is a sequence (YAML array).
     * @return True if sequence, false otherwise.
     */
    bool IsSequence() const noexcept { return kind_ == Kind::Sequence; }

    /**
     * @brief Returns true if the node is a mapping (YAML dictionary).
     * @return True if mapping, false otherwise.
     */
    bool IsMapping() const noexcept { return kind_ == Kind::Mapping; }

    /**
     * @brief Returns the contained boolean value.
     * @throws std::runtime_error if not a boolean node.
     * @return Boolean value.
     */
    bool AsBool() const { return std::get<bool>(AssertKind(Kind::Bool)); }

    /**
     * @brief Returns the contained integer value.
     * @throws std::runtime_error if not an integer node.
     * @return Integer value.
     */
    int64_t AsInt() const { return std::get<int64_t>(AssertKind(Kind::Int)); }

    /**
     * @brief Returns the contained double value.
     * @throws std::runtime_error if not a double node.
     * @return Double value.
     */
    double AsDouble() const { return std::get<double>(AssertKind(Kind::Double)); }

    /**
     * @brief Returns the contained string value.
     * @throws std::runtime_error if not a string node.
     * @return String value.
     */
    const std::string& AsString() const { return std::get<std::string>(AssertKind(Kind::String)); }

    /**
     * @brief Returns the contained sequence value.
     * @throws std::runtime_error if not a sequence node.
     * @return Sequence of nodes.
     */
    const std::vector<Node>& AsSequence() const { return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)); }

    /**
     * @brief Returns the contained mapping value.
     * @throws std::runtime_error if not a mapping node.
     * @return Mapping of string to node.
     */
    const std::map<std::string, Node>& AsMapping() const { return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping)); }

    /**
     * @brief Const mapping lookup operator.
     * @param key The key to look up.
     * @throws std::runtime_error if not a mapping, std::out_of_range if key not found.
     * @return Const reference to value node.
     */
    const Node& operator[](const std::string& key) const {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping)).at(key);
    }

    /**
     * @brief Mutable mapping lookup operator (inserts null if key missing).
     * @param key The key to look up.
     * @throws std::runtime_error if not a mapping.
     * @return Reference to value node.
     */
    Node& operator[](const std::string& key) {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping))[key];
    }

    /**
     * @brief Const sequence lookup operator.
     * @param idx Index in sequence.
     * @throws std::runtime_error if not a sequence, std::out_of_range if index invalid.
     * @return Const reference to element node.
     */
    const Node& operator[](size_t idx) const {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)).at(idx);
    }

    /**
     * @brief Mutable sequence lookup operator.
     * @param idx Index in sequence.
     * @throws std::runtime_error if not a sequence, std::out_of_range if index invalid.
     * @return Reference to element node.
     */
    Node& operator[](size_t idx) {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence))[idx];
    }

    /**
     * @brief Try to convert to type T, or return std::nullopt on type mismatch.
     * @tparam T Target type (bool, int64_t, double, std::string)
     * @return Optional value.
     */
    template<typename T>
    std::optional<T> TryAs() const;

private:
    /**
     * @brief Check that this node is of the expected kind.
     * @param expected The expected Kind.
     * @throws std::runtime_error if the kind does not match.
     * @return Const reference to value.
     */
    const Value& AssertKind(Kind expected) const {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " +
                KindToStr(expected) + ", got " + KindToStr(kind_) + ")");
        return value_;
    }

    /**
     * @brief Check that this node is of the expected kind (mutable).
     * @param expected The expected Kind.
     * @throws std::runtime_error if the kind does not match.
     * @return Reference to value.
     */
    Value& AssertKind(Kind expected) {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " +
                KindToStr(expected) + ", got " + KindToStr(kind_) + ")");
        return value_;
    }

    /**
     * @brief Convert Kind to a string for error messages.
     * @param k Node kind.
     * @return String representation.
     */
    static std::string KindToStr(Kind k) {
        switch (k) {
            case Kind::Null:     return "Null";
            case Kind::Bool:     return "Bool";
            case Kind::Int:      return "Int";
            case Kind::Double:   return "Double";
            case Kind::String:   return "String";
            case Kind::Sequence: return "Sequence";
            case Kind::Mapping:  return "Mapping";
        }
        return "Unknown";
    }

    Kind kind_;   /**< The kind of this node. */
    Value value_; /**< The value of this node. */
};

/// @cond INTERNAL
// Template specializations for TryAs for each scalar type.
template<> inline std::optional<bool> Node::TryAs<bool>() const {
    if (kind_ == Kind::Bool) return std::get<bool>(value_);
    return std::nullopt;
}
template<> inline std::optional<int64_t> Node::TryAs<int64_t>() const {
    if (kind_ == Kind::Int) return std::get<int64_t>(value_);
    return std::nullopt;
}
template<> inline std::optional<double> Node::TryAs<double>() const {
    if (kind_ == Kind::Double) return std::get<double>(value_);
    return std::nullopt;
}
template<> inline std::optional<std::string> Node::TryAs<std::string>() const {
    if (kind_ == Kind::String) return std::get<std::string>(value_);
    return std::nullopt;
}
/// @endcond

/**
 * @class Parser
 * @brief Internal YAML parser. Use YAML::ParseString or YAML::ParseFile instead.
 *
 * Supports block style mapping and sequence, multi-line scalars, anchors, aliases, and merge keys.
 */
class Parser {
public:
    /**
     * @brief Parse a YAML document from a string.
     * @param input The YAML document as a string_view.
     * @return The parsed root Node.
     * @throws std::runtime_error on parse error.
     */
    static Node ParseString(std::string_view input) {
        Parser p(input);
        return p.ParseDocument();
    }

private:
    /**
     * @struct LineInfo
     * @brief Represents a single preprocessed YAML line.
     *
     * Stores indentation, content, and line number.
     */
    struct LineInfo {
        size_t indent;            /**< Number of leading spaces. */
        std::string_view content; /**< Line content after indentation. */
        size_t line_no;           /**< 1-based line number. */
    };

    std::vector<LineInfo> lines_;             /**< Non-empty, non-comment YAML lines. */
    size_t cur_ = 0;                          /**< Current parse position (index in lines_). */
    std::unordered_map<std::string, Node> anchors_; /**< Table of anchor names to Node values. */

    /**
     * @brief Constructor. Tokenizes input into preprocessed lines.
     * @param input The YAML document.
     */
    Parser(std::string_view input) {
        size_t off = 0, ln = 1;
        while (off < input.size()) {
            size_t e = input.find_first_of("\r\n", off);
            if (e == std::string_view::npos) e = input.size();
            auto raw = input.substr(off, e - off);
            size_t sp = 0; while (sp < raw.size() && raw[sp] == ' ') ++sp;
            auto ct = raw.substr(sp);
            if (!ct.empty() && ct[0] != '#')
                lines_.push_back({sp, ct, ln});
            off = (e == input.size()) ? input.size()
                : (input[e]=='\r'&&e+1<input.size()&&input[e+1]=='\n' ? e+2 : e+1);
            ++ln;
        }
    }

    /**
     * @brief Parse an entire YAML document starting at the current line.
     * @return The root Node.
     */
    Node ParseDocument() {
        if (lines_.empty()) return Node();
        return ParseBlock(0);
    }

    /**
     * @brief Parse a block at the specified indentation (mapping or sequence).
     * @param base_indent Required indentation for block.
     * @return Node representing the block.
     */
    Node ParseBlock(size_t base_indent) {
        return lines_[cur_].content.starts_with('-')
             ? ParseSequence(base_indent)
             : ParseMapping(base_indent);
    }

    /**
     * @brief Extract an anchor marker (&name) if present.
     * @param s Line segment.
     * @return Pair: anchor name (empty if none), and remainder of string.
     */
    static std::pair<std::string, std::string_view> parseAnchor(std::string_view s) {
        auto pos = s.find('&');
        if (pos==std::string_view::npos) return {"",s};
        size_t i = pos+1;
        while (i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_'||s[i]=='-')) ++i;
        std::string name(s.substr(pos+1,i-(pos+1)));
        std::string_view rest = (i<s.size()&&s[i]==' ')
            ? s.substr(i+1) : s.substr(i);
        return {name,rest};
    }

    /**
     * @brief Extract an alias marker (*name) if present.
     * @param s Line segment.
     * @return Pair: alias name (empty if none), and boolean if found.
     */
    static std::pair<std::string, bool> parseAlias(std::string_view s) {
        auto pos = s.find('*');
        if (pos==std::string_view::npos) return {"",false};
        size_t i = pos+1;
        while (i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_'||s[i]=='-')) ++i;
        return {std::string(s.substr(pos+1,i-(pos+1))), true};
    }

    /**
     * @brief Parse a YAML mapping (dictionary/object) block at the current indentation.
     *        Registers anchors after parsing each value; supports aliases, merge keys, multi-line scalars.
     * @param base_indent Required indentation for this block.
     * @return Mapping node.
     */
    Node ParseMapping(size_t base_indent) {
        std::map<std::string,Node> m;
        while (cur_<lines_.size() && lines_[cur_].indent>=base_indent) {
            auto &li = lines_[cur_];
            auto col = li.content.find(':');
            if (col==std::string_view::npos)
                throw std::runtime_error("YAML: missing ':' at line "+std::to_string(li.line_no));
            // Key + key-anchor
            auto kv = Trim(li.content.substr(0,col));
            auto [kanchor,kbase] = parseAnchor(kv);
            std::string key(kbase);
            // Remainder + value-anchor
            auto rest = li.content.substr(col+1);
            size_t sp=0; while (sp<rest.size()&&rest[sp]==' ') ++sp;
            rest = rest.substr(sp);
            auto [vanchor,vrem] = parseAnchor(rest);
            std::string val_anchor = vanchor;
            std::string_view content = vrem;

            Node val;
            // Multi-line scalar
            if (content=="|"||content==">") {
                char t = content[0]; ++cur_;
                val = ParseMultilineScalar(li.indent+2,t);
            }
            // Alias or merge
            else if (auto [al, ia] = parseAlias(content); ia) {
                if (!anchors_.count(al))
                    throw std::runtime_error("YAML: undefined alias '*"+al+"' at line "+std::to_string(li.line_no));
                val = anchors_[al];
                if (key=="<<") {
                    if (!val.IsMapping())
                        throw std::runtime_error("YAML: merge key '*"+al+"' not mapping at line "+std::to_string(li.line_no));
                    for (auto &kv2: val.AsMapping()) m.insert(kv2);
                    ++cur_;
                    continue;
                }
                ++cur_;
            }
            // Nested block or null
            else if (content.empty()) {
                ++cur_;
                if (cur_<lines_.size()&&lines_[cur_].indent>li.indent)
                    val = ParseBlock(li.indent+2);
                else
                    val = Node();
            }
            // Plain scalar
            else {
                ++cur_;
                val = ParseScalar(content);
            }

            // Register anchors now
            if (!kanchor.empty())   anchors_[kanchor]   = val;
            if (!val_anchor.empty()) anchors_[val_anchor] = val;
            m.emplace(std::move(key), std::move(val));
        }
        return Node(std::move(m));
    }

    /**
     * @brief Parse a YAML sequence (list/array) block at the current indentation.
     *        Supports anchors and aliases on elements.
     * @param base_indent Required indentation for this block.
     * @return Sequence node.
     */
    Node ParseSequence(size_t base_indent) {
        std::vector<Node> seq;
        while (cur_<lines_.size()
            && lines_[cur_].indent>=base_indent
            && lines_[cur_].content.starts_with('-'))
        {
            auto &li = lines_[cur_];
            auto rest = li.content.substr(1);
            size_t sp=0; while (sp<rest.size()&&rest[sp]==' ') ++sp;
            rest = rest.substr(sp);
            auto [anchor,rem] = parseAnchor(rest);
            std::string elt_anchor = anchor;
            std::string_view content = rem;

            Node elt;
            if (content.empty()) {
                ++cur_;
                elt = ParseBlock(li.indent+2);
            }
            else if (content=="|"||content==">") {
                char t=content[0]; ++cur_;
                elt = ParseMultilineScalar(li.indent+2,t);
            }
            else if (auto [al, ia] = parseAlias(content); ia) {
                if (!anchors_.count(al))
                    throw std::runtime_error("YAML: undefined alias '*"+al+"' in sequence at line "+std::to_string(li.line_no));
                elt = anchors_[al];
                ++cur_;
            }
            else {
                ++cur_;
                elt = ParseScalar(content);
            }
            if (!elt_anchor.empty())
                anchors_[elt_anchor] = elt;
            seq.push_back(std::move(elt));
        }
        return Node(std::move(seq));
    }

    /**
     * @brief Parse a YAML multi-line scalar block (literal '|' or folded '>').
     * @param indent Indentation level for this scalar.
     * @param type '|' for literal, '>' for folded.
     * @return Node containing the parsed string.
     */
    Node ParseMultilineScalar(size_t indent, char type) {
        std::string out;
        bool first=true;
        while (cur_<lines_.size() && lines_[cur_].indent>=indent) {
            auto line = lines_[cur_].content;
            if (!first) {
                if (type=='|') out.push_back('\n');
                else           out.push_back(line.empty()?'\n':' ');
            }
            out += line;
            first = false;
            ++cur_;
        }
        return Node(std::move(out));
    }

    /**
     * @brief Parse a single-line scalar value.
     *        Handles null, bool, int, double, quoted, and plain strings.
     * @param s String to parse.
     * @return Scalar node.
     */
    static Node ParseScalar(std::string_view s) {
        if (s=="~"||s=="null"||s=="Null"||s=="NULL") return Node();
        if (s=="true"||s=="True"||s=="TRUE")       return Node(true);
        if (s=="false"||s=="False"||s=="FALSE")    return Node(false);
        int64_t i=0;
        if (auto [p,ec]=std::from_chars(s.data(),s.data()+s.size(),i);
            ec==std::errc() && p==s.data()+s.size()) return Node(i);
        double d=0;
        if (auto [p2,ec2]=std::from_chars(s.data(),s.data()+s.size(),d);
            ec2==std::errc() && p2==s.data()+s.size()) return Node(d);
        if ((s.starts_with('"')&&s.ends_with('"')) ||
            (s.starts_with('\'')&&s.ends_with('\'')))
            return Node(std::string(s.substr(1,s.size()-2)));
        return Node(std::string(s));
    }

    /**
     * @brief Trim whitespace from both ends of a string_view.
     * @param s String to trim.
     * @return Trimmed string_view.
     */
    static std::string_view Trim(std::string_view s) {
        size_t b=0,e=s.size();
        while(b<e && std::isspace((unsigned char)s[b])) ++b;
        while(e>b && std::isspace((unsigned char)s[e-1])) --e;
        return s.substr(b,e-b);
    }
};

/**
 * @brief Parse a YAML document from a string.
 * @param input YAML text as std::string_view.
 * @return Root node.
 * @throws std::runtime_error on parse error.
 */
inline Node ParseString(std::string_view input) {
    return Parser::ParseString(input);
}

/**
 * @brief Parse a YAML document from a file.
 * @param filename Path to YAML file.
 * @return Root node.
 * @throws std::runtime_error if file cannot be opened or parsed.
 */
inline Node ParseFile(const std::string& filename) {
    std::ifstream f(filename);
    if (!f) throw std::runtime_error("YAML: cannot open file: "+filename);
    std::stringstream ss; ss<<f.rdbuf();
    return ParseString(ss.str());
}

} // namespace YAML

#endif // YAML_BUTTER_HPP
