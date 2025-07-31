/**
 * @file yaml_butter.hpp
 * @brief Single-header YAML parser for C++23 with full block and flow style support.
 *
 * This header-only YAML parser supports:
 *   - Block-style Mappings (dictionary/object), Sequences (array/list), and Scalars (string, int, double, bool, null)
 *   - Flow-style Mappings (`{a:1, b:2}`) and Sequences (`[x,y,z]`)
 *   - Indentation-based parsing (block style YAML)
 *   - Multi-line scalars (literal '|' and folded '>')
 *   - Anchors and aliases (&anchor, *alias) for mappings, sequences, and scalars
 *   - Merge keys (`<<: *anchor`) for mappings
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

namespace YAML {

/**
 * @class Node
 * @brief Represents any YAML node: Null, Bool, Int, Double, String, Sequence, or Mapping.
 *
 * This is the primary data structure used to represent all values loaded from YAML files.
 * Each YAML node (mapping, sequence, scalar) is dynamically typed at runtime.
 */
class Node {
public:
    /** Enumeration of node kinds. */
    enum class Kind {
        Null,      ///< Represents an explicit null value.
        Bool,      ///< Boolean value (true/false).
        Int,       ///< Integer value.
        Double,    ///< Floating-point (double) value.
        String,    ///< Text scalar value.
        Sequence,  ///< Sequence (vector/list) of nodes.
        Mapping    ///< Mapping (map/dictionary) of string-to-node.
    };

    /** 
     * @brief Type-erased value for this node.
     * The variant stores the active value for this node, based on Kind.
     */
    using Value = std::variant<
        std::monostate,            ///< Null type (empty).
        bool,                      ///< Boolean type.
        int64_t,                   ///< Integer type.
        double,                    ///< Floating-point type.
        std::string,               ///< String type.
        std::vector<Node>,         ///< Sequence type.
        std::map<std::string, Node>///< Mapping type.
    >;

    /** @brief Construct a Null node by default. */
    Node() noexcept : kind_(Kind::Null), value_(std::monostate{}) {}

    /** @brief Construct a Bool node. */
    Node(bool b) noexcept : kind_(Kind::Bool), value_(b) {}

    /** @brief Construct an Int node. */
    Node(int64_t i) noexcept : kind_(Kind::Int), value_(i) {}

    /** @brief Construct a Double node. */
    Node(double d) noexcept : kind_(Kind::Double), value_(d) {}

    /** @brief Construct a String node from std::string. */
    Node(std::string s) noexcept : kind_(Kind::String), value_(std::move(s)) {}

    /** @brief Construct a String node from a C-string. */
    Node(const char* s) : kind_(Kind::String), value_(std::string(s)) {}

    /** @brief Construct a Sequence node. */
    Node(std::vector<Node> seq) noexcept : kind_(Kind::Sequence), value_(std::move(seq)) {}

    /** @brief Construct a Mapping node. */
    Node(std::map<std::string, Node> map) noexcept : kind_(Kind::Mapping), value_(std::move(map)) {}

    /** @brief Returns the runtime kind (type) of this node. */
    Kind Type()    const noexcept { return kind_; }

    /** @brief Returns true if this node is Null. */
    bool IsNull()  const noexcept { return kind_ == Kind::Null; }

    /** @brief Returns true if this node is any scalar (bool, int, double, or string). */
    bool IsScalar()const noexcept { return kind_ == Kind::Bool || kind_ == Kind::Int || kind_ == Kind::Double || kind_ == Kind::String; }

    /** @brief Returns true if this node is a Sequence (YAML array/list). */
    bool IsSequence() const noexcept { return kind_ == Kind::Sequence; }

    /** @brief Returns true if this node is a Mapping (YAML map/dictionary/object). */
    bool IsMapping()  const noexcept { return kind_ == Kind::Mapping; }

    /** @brief Return the value as bool. Throws on wrong type. */
    bool AsBool()   const { return std::get<bool>(AssertKind(Kind::Bool)); }

    /** @brief Return the value as int64_t. Throws on wrong type. */
    int64_t AsInt() const { return std::get<int64_t>(AssertKind(Kind::Int)); }

    /** @brief Return the value as double. Throws on wrong type. */
    double AsDouble() const { return std::get<double>(AssertKind(Kind::Double)); }

    /** @brief Return the value as const std::string&. Throws on wrong type. */
    const std::string& AsString() const { return std::get<std::string>(AssertKind(Kind::String)); }

    /** @brief Return the value as const std::vector<Node>&. Throws on wrong type. */
    const std::vector<Node>& AsSequence() const { return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)); }

    /** @brief Return the value as const std::map<std::string, Node>&. Throws on wrong type. */
    const std::map<std::string, Node>& AsMapping()  const { return std::get<std::map<std::string,Node>>(AssertKind(Kind::Mapping)); }

    /** @brief Access mapping node by key (const). Throws if not a mapping. */
    const Node& operator[](const std::string& key) const {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping)).at(key);
    }

    /** @brief Access mapping node by key (mutable). Throws if not a mapping. */
    Node& operator[](const std::string& key) {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping))[key];
    }

    /** @brief Access sequence node by index (const). Throws if not a sequence. */
    const Node& operator[](size_t idx) const {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)).at(idx);
    }

    /** @brief Access sequence node by index (mutable). Throws if not a sequence. */
    Node& operator[](size_t idx) {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)).at(idx);
    }

    /**
     * @brief Try to convert the node value to a specific scalar type.
     * @tparam T bool, int64_t, double, or std::string
     * @return std::optional<T>: the converted value, or std::nullopt if not possible.
     */
    template<typename T>
    std::optional<T> TryAs() const;

private:
    /**
     * @brief Helper for runtime type-checking.
     * Throws if the node's kind does not match the expected kind.
     * @param expected The expected node Kind.
     * @return Reference to the stored value (const).
     */
    const Value& AssertKind(Kind expected) const {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " + KindToStr(expected) +
                                     ", got " + KindToStr(kind_) + ")");
        return value_;
    }
    /**
     * @brief Mutable version of AssertKind for internal mutation.
     */
    Value& AssertKind(Kind expected) {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " + KindToStr(expected) +
                                     ", got " + KindToStr(kind_) + ")");
        return value_;
    }
    /**
     * @brief Convert a Kind enum value to a human-readable string.
     * @param k The Kind value.
     * @return String representation of Kind.
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

    Kind   kind_;   ///< The current type of this node.
    Value  value_;  ///< The value stored for this node.
};

/// @cond INTERNAL
/**
 * @brief Specialized TryAs for bool type.
 */
template<> inline std::optional<bool> Node::TryAs<bool>() const {
    if (kind_ == Kind::Bool) return std::get<bool>(value_);
    return std::nullopt;
}
/**
 * @brief Specialized TryAs for int64_t type.
 */
template<> inline std::optional<int64_t> Node::TryAs<int64_t>() const {
    if (kind_ == Kind::Int) return std::get<int64_t>(value_);
    return std::nullopt;
}
/**
 * @brief Specialized TryAs for double type.
 */
template<> inline std::optional<double> Node::TryAs<double>() const {
    if (kind_ == Kind::Double) return std::get<double>(value_);
    return std::nullopt;
}
/**
 * @brief Specialized TryAs for std::string type.
 */
template<> inline std::optional<std::string> Node::TryAs<std::string>() const {
    if (kind_ == Kind::String) return std::get<std::string>(value_);
    return std::nullopt;
}
/// @endcond

/**
 * @class Parser
 * @brief Core YAML parser: performs all parsing logic, anchor/alias/merge tracking, and document construction.
 *
 * This is a single-use parsing engine: constructed on a YAML input, and used to produce a `Node` representing the document.
 * Not thread-safe, but can be called concurrently on different documents/files.
 */
class Parser {
public:
    /**
     * @brief Parse YAML from a string input and produce a root node.
     * @param input The YAML text as a std::string_view.
     * @return The parsed YAML as a Node.
     * @throws std::runtime_error on malformed YAML or invalid structure.
     */
    static Node ParseString(std::string_view input) {
        Parser p(input);
        return p.ParseDocument();
    }

private:
    /**
     * @struct LineInfo
     * @brief Describes a logical, preprocessed YAML line: leading indentation, line content, and line number.
     *
     * This helps support indentation-based block parsing and more robust error messages.
     */
    struct LineInfo {
        size_t indent;            ///< Number of leading spaces for this line.
        std::string_view content; ///< Content of line after indentation (no trailing newline).
        size_t line_no;           ///< 1-based line number from source.
    };

    std::vector<LineInfo> lines_;               ///< All meaningful YAML lines (not blank/comment).
    size_t cur_ = 0;                            ///< Index into current line during parsing.
    std::unordered_map<std::string, Node> anchors_; ///< Table of anchors defined in this document.

    /**
     * @brief Constructor: Preprocess YAML into non-comment, non-empty lines, recording indent and line numbers.
     *        This enables easier block parsing and detailed error reporting.
     */
    Parser(std::string_view input) {
        size_t off = 0, ln = 1;
        while (off < input.size()) {
            size_t e = input.find_first_of("\r\n", off);
            if (e == std::string_view::npos) e = input.size();
            auto raw = input.substr(off, e - off);
            size_t sp = 0; while (sp < raw.size() && raw[sp]==' ') ++sp;
            auto ct = raw.substr(sp);
            if (!ct.empty() && ct[0] != '#')
                lines_.push_back({sp, ct, ln});
            off = (e == input.size()) ? input.size()
                  : (input[e]=='\r' && e+1<input.size() && input[e+1]=='\n' ? e+2 : e+1);
            ++ln;
        }
    }

    /**
     * @brief Entry point: Parse the document starting from the first logical YAML line.
     * @return Node representing the full YAML document (mapping, sequence, scalar).
     */
    Node ParseDocument() {
        if (lines_.empty()) return Node();
        return ParseBlock(0);
    }

    /**
     * @brief Dispatch block parsing logic based on indentation and line contents.
     *        Handles block-style, flow-style, and sequence nodes at the root or sub-blocks.
     * @param base_indent The indentation level this block expects.
     * @return The parsed YAML node for this block.
     */
    Node ParseBlock(size_t base_indent) {
        auto& c = lines_[cur_].content;
        if (c.starts_with('-')) {
            // Block-style YAML sequence node (starts with '-').
            return ParseSequence(base_indent);
        } else if (c.starts_with('[')) {
            // Flow-style YAML sequence node ([ ... ]).
            auto close = c.find_last_of(']');
            if (close != std::string::npos)
                return ParseFlowSequence(c.substr(1, close - 1));
            throw std::runtime_error("YAML: Unterminated flow sequence");
        } else if (c.starts_with('{')) {
            // Flow-style YAML mapping node ({ ... }).
            auto close = c.find_last_of('}');
            if (close != std::string::npos)
                return ParseFlowMapping(c.substr(1, close - 1));
            throw std::runtime_error("YAML: Unterminated flow mapping");
        } else {
            // Block-style YAML mapping node (typical object).
            return ParseMapping(base_indent);
        }
    }

    /**
     * @brief Parse any anchor (&anchor) in a line, returning anchor name and the remaining string.
     * @param s The input string to search for an anchor in.
     * @return {anchor name (or ""), rest of line after anchor}.
     *
     * If no anchor is present, returns {"", s}.
     */
    static std::pair<std::string,std::string_view> parseAnchor(std::string_view s) {
        auto pos = s.find('&');
        if (pos == std::string_view::npos) return {"",s};
        size_t i = pos+1;
        while (i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_'||s[i]=='-')) ++i;
        std::string name(s.substr(pos+1, i-(pos+1)));
        std::string_view rest = (i<s.size()&&s[i]==' ') ? s.substr(i+1) : s.substr(i);
        return {name,rest};
    }

    /**
     * @brief Parse any alias (*alias) in a line, returning alias name and success flag.
     * @param s The input string to search for an alias in.
     * @return {alias name (or ""), whether an alias was found}.
     *
     * If no alias is present, returns {"", false}.
     */
    static std::pair<std::string,bool> parseAlias(std::string_view s) {
        auto pos = s.find('*');
        if (pos == std::string_view::npos) return {"",false};
        size_t i = pos+1;
        while (i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_'||s[i]=='-')) ++i;
        return {std::string(s.substr(pos+1, i-(pos+1))), true};
    }

    /**
     * @brief Parse a block-style YAML mapping (dictionary/object) at a given indent.
     *
     * Handles anchors, aliases, merge keys (<<: *anchor), and both block/flow-style values.
     *
     * @param base_indent The indentation level expected for this mapping block.
     * @return A Node of kind Mapping with all keys/values parsed.
     */
    Node ParseMapping(size_t base_indent) {
        std::map<std::string,Node> m;
        while (cur_ < lines_.size() && lines_[cur_].indent >= base_indent) {
            auto& li = lines_[cur_];
            auto pos = li.content.find(':');
            if (pos == std::string::npos)
                throw std::runtime_error("YAML: missing ':' at line "+std::to_string(li.line_no));
            // Parse key and possible key anchor
            auto keyv = Trim(li.content.substr(0,pos));
            auto [kanchor, kbase] = parseAnchor(keyv);
            std::string key(kbase);
            // Parse possible anchor/value for this value
            auto rest = li.content.substr(pos+1);
            size_t sp=0; while(sp<rest.size()&&rest[sp]==' ')++sp;
            rest = rest.substr(sp);
            auto [vanchor, vrem] = parseAnchor(rest);
            std::string val_anchor = vanchor;
            std::string_view content = vrem;

            Node val;
            // Case 1: Multi-line scalar value (| or >) as value
            if (content == "|" || content == ">") {
                char t = content[0]; ++cur_;
                val = ParseMultilineScalar(li.indent+2, t);
            }
            // Case 2: Alias or merge key
            else if (auto [a, ia] = parseAlias(content); ia) {
                if (!anchors_.count(a))
                    throw std::runtime_error("YAML: undefined alias '*"+a+"' at line "+std::to_string(li.line_no));
                val = anchors_[a];
                if (key=="<<") {
                    // Merge mapping into current map
                    if (!val.IsMapping())
                        throw std::runtime_error("YAML: merge key '*"+a+"' not mapping");
                    for(auto& kv: val.AsMapping()) m.insert(kv);
                    ++cur_; continue;
                }
                ++cur_;
            }
            // Case 3: Flow-style mapping or sequence as value
            else if (content.starts_with('{')) {
                val = ParseFlowMapping(content.substr(1, content.size()-2));
                ++cur_;
            }
            else if (content.starts_with('[')) {
                val = ParseFlowSequence(content.substr(1, content.size()-2));
                ++cur_;
            }
            // Case 4: Nested block mapping or empty/null value
            else if (content.empty()) {
                ++cur_;
                if (cur_<lines_.size() && lines_[cur_].indent>li.indent)
                    val = ParseBlock(li.indent+2);
                else
                    val = Node();
            }
            // Case 5: Plain scalar value
            else {
                ++cur_;
                val = ParseScalar(content);
            }

            // Anchor management: allow anchors on keys and values
            if (!kanchor.empty())   anchors_[kanchor]   = val;
            if (!val_anchor.empty()) anchors_[val_anchor] = val;
            m.emplace(std::move(key), std::move(val));
        }
        return Node(std::move(m));
    }

    /**
     * @brief Parse a block-style YAML sequence (array/list) at a given indent.
     *
     * Handles anchors, aliases, multi-line and flow-style elements, and nested blocks.
     *
     * @param base_indent The indentation level expected for this sequence block.
     * @return A Node of kind Sequence containing all parsed elements.
     */
    Node ParseSequence(size_t base_indent) {
        std::vector<Node> seq;
        while (cur_ < lines_.size() &&
               lines_[cur_].indent >= base_indent &&
               lines_[cur_].content.starts_with('-'))
        {
            auto& li = lines_[cur_];
            auto rest = li.content.substr(1);
            size_t sp=0; while(sp<rest.size()&&rest[sp]==' ')++sp;
            rest = rest.substr(sp);
            auto [anchor, rem] = parseAnchor(rest);
            std::string elt_anchor = anchor;
            std::string_view content = rem;

            Node elt;
            // Nested block sequence element
            if (content.empty()) {
                ++cur_;
                elt = ParseBlock(li.indent+2);
            }
            // Multi-line scalar as element
            else if (content == "|" || content == ">") {
                char t = content[0]; ++cur_;
                elt = ParseMultilineScalar(li.indent+2, t);
            }
            // Alias reference as element
            else if (auto [a, ia] = parseAlias(content); ia) {
                if (!anchors_.count(a))
                    throw std::runtime_error("YAML: undefined alias '*"+a+"' in sequence");
                elt = anchors_[a];
                ++cur_;
            }
            // Flow-style mapping or sequence as element
            else if (content.starts_with('{')) {
                elt = ParseFlowMapping(content.substr(1, content.size()-2));
                ++cur_;
            }
            else if (content.starts_with('[')) {
                elt = ParseFlowSequence(content.substr(1, content.size()-2));
                ++cur_;
            }
            // Scalar element
            else {
                ++cur_;
                elt = ParseScalar(content);
            }

            // Store anchors for sequence elements, if present
            if (!elt_anchor.empty()) anchors_[elt_anchor] = elt;
            seq.push_back(std::move(elt));
        }
        return Node(std::move(seq));
    }

    /**
     * @brief Parse a YAML multi-line scalar value, either as a literal (|) or folded (>).
     *
     * This reads lines at the given indent or greater, preserving line breaks as specified by YAML.
     *
     * @param indent Required indentation for each line of the scalar block.
     * @param type '|' for literal, '>' for folded.
     * @return A Node of kind String containing the full multi-line value.
     */
    Node ParseMultilineScalar(size_t indent, char type) {
        std::string out;
        bool first = true;
        while (cur_ < lines_.size() && lines_[cur_].indent >= indent) {
            auto line = lines_[cur_].content;
            if (!first) {
                if (type=='|') out.push_back('\n'); // Literal: preserve newlines
                else            out.push_back(line.empty()?'\n':' '); // Folded: wrap unless blank
            }
            out += line;
            first = false;
            ++cur_;
        }
        return Node(std::move(out));
    }

    /**
     * @brief Parse a single-line YAML scalar, which may be null, bool, int, double, quoted, or plain.
     *
     * This function is robust to YAML’s flexible scalar syntax.
     * @param s The string to parse as a scalar.
     * @return A Node of scalar kind (Null, Bool, Int, Double, or String).
     */
    static Node ParseScalar(std::string_view s) {
        // Null scalars (various YAML spellings)
        if (s=="~"||s=="null"||s=="Null"||s=="NULL")     return Node();
        // Boolean scalars (various spellings)
        if (s=="true"||s=="True"||s=="TRUE")             return Node(true);
        if (s=="false"||s=="False"||s=="FALSE")          return Node(false);
        // Integer scalar
        int64_t i=0;
        if (auto [p,ec] = std::from_chars(s.data(),s.data()+s.size(),i);
            ec==std::errc() && p==s.data()+s.size())
            return Node(i);
        // Double scalar
        double d=0;
        if (auto [p2,ec2] = std::from_chars(s.data(),s.data()+s.size(),d);
            ec2==std::errc() && p2==s.data()+s.size())
            return Node(d);
        // Quoted string (single or double)
        if ((s.starts_with('"') && s.ends_with('"')) ||
            (s.starts_with('\'') && s.ends_with('\'')))
            return Node(std::string(s.substr(1,s.size()-2)));
        // Default: plain string
        return Node(std::string(s));
    }

    /**
     * @brief Trim whitespace from both the left and right ends of a string_view.
     *        Used for robust parsing of keys, values, and elements.
     * @param s Input string_view.
     * @return Trimmed string_view.
     */
    static std::string_view Trim(std::string_view s) {
        size_t b=0, e=s.size();
        while (b<e && std::isspace((unsigned char)s[b])) ++b;
        while (e>b && std::isspace((unsigned char)s[e-1])) --e;
        return s.substr(b, e-b);
    }

    /**
     * @brief Parse a YAML flow-style sequence ([a, b, c]).
     *        Handles nesting, strings, numbers, and booleans.
     * @param s Contents between [ and ] (not including brackets themselves).
     * @return Node of kind Sequence.
     */
    static Node ParseFlowSequence(std::string_view s) {
        std::vector<Node> seq;
        size_t i = 0;
        while (i < s.size()) {
            while (i<s.size() && (std::isspace((unsigned char)s[i])||s[i]==',')) ++i;
            if (i>=s.size() || s[i]==']') break;
            size_t start=i, depth=0;
            bool in_q=false; char qc=0;
            while (i<s.size()) {
                if (!in_q && (s[i]==','||s[i]==']')&&depth==0) break;
                if (!in_q && (s[i]=='['||s[i]=='{')) ++depth;
                else if (!in_q && (s[i]==']'||s[i]=='}')) --depth;
                else if (s[i]=='\''||s[i]=='"') {
                    if (!in_q) { in_q=true; qc=s[i]; }
                    else if (in_q&&s[i]==qc) in_q=false;
                }
                ++i;
            }
            auto tok = Trim(s.substr(start,i-start));
            if (tok.starts_with('['))
                seq.push_back(ParseFlowSequence(tok.substr(1,tok.size()-2)));
            else if (tok.starts_with('{'))
                seq.push_back(ParseFlowMapping(tok.substr(1,tok.size()-2)));
            else
                seq.push_back(ParseScalar(tok));
            while (i<s.size()&&(s[i]==','||std::isspace((unsigned char)s[i]))) ++i;
        }
        return Node(std::move(seq));
    }

    /**
     * @brief Parse a YAML flow-style mapping ({foo: 1, bar: 2}).
     *        Handles nested flow structures, booleans, numbers, and quoted strings.
     * @param s Contents between { and } (not including braces themselves).
     * @return Node of kind Mapping.
     */
    static Node ParseFlowMapping(std::string_view s) {
        std::map<std::string,Node> map;
        size_t i = 0;
        while (i < s.size()) {
            while (i<s.size()&&(std::isspace((unsigned char)s[i])||s[i]==',')) ++i;
            if (i>=s.size()||s[i]=='}') break;
            size_t start=i;
            while (i<s.size()&&s[i]!=':'&&s[i]!='}') ++i;
            if (i>=s.size()||s[i]=='}') break;
            auto key = Trim(s.substr(start,i-start));
            ++i; while (i<s.size()&&std::isspace((unsigned char)s[i])) ++i;
            start=i; size_t depth=0; bool in_q=false; char qc=0;
            while (i<s.size()) {
                if (!in_q&&(s[i]==','||s[i]=='}')&&depth==0) break;
                if (!in_q&&(s[i]=='['||s[i]=='{')) ++depth;
                else if (!in_q&&(s[i]==']'||s[i]=='}')) --depth;
                else if (s[i]=='\''||s[i]=='"') {
                    if (!in_q) { in_q=true; qc=s[i]; }
                    else if (in_q&&s[i]==qc) in_q=false;
                }
                ++i;
            }
            auto val = Trim(s.substr(start,i-start));
            if (val.starts_with('['))
                map[std::string(key)] = ParseFlowSequence(val.substr(1,val.size()-2));
            else if (val.starts_with('{'))
                map[std::string(key)] = ParseFlowMapping(val.substr(1,val.size()-2));
            else
                map[std::string(key)] = ParseScalar(val);
            while (i<s.size()&&(s[i]==','||std::isspace((unsigned char)s[i]))) ++i;
        }
        return Node(std::move(map));
    }
};

/**
 * @brief Shorthand for parsing a YAML document from a string.
 * @param in The YAML input as a string_view.
 * @return Parsed YAML root node.
 */
inline Node ParseString(std::string_view in) {
    return Parser::ParseString(in);
}

/**
 * @brief Shorthand for parsing a YAML document directly from a file.
 * @param fn The filename to open.
 * @return Parsed YAML root node.
 * @throws std::runtime_error if the file cannot be opened.
 */
inline Node ParseFile(const std::string& fn) {
    std::ifstream f(fn);
    if (!f) throw std::runtime_error("YAML: cannot open file: "+fn);
    std::stringstream ss; ss<<f.rdbuf();
    return ParseString(ss.str());
}

} // namespace YAML

#endif // YAML_BUTTER_HPP
