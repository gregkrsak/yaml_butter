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
 * @author Greg
 * @author ChatGPT
 * @date 2024
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
 */
class Node {
public:
    /** Enumeration of node kinds. */
    enum class Kind { Null, Bool, Int, Double, String, Sequence, Mapping };

    /** Underlying variant for node value. */
    using Value = std::variant<
        std::monostate,            /**< Null */
        bool,                      /**< Boolean */
        int64_t,                   /**< Integer */
        double,                    /**< Double */
        std::string,               /**< String */
        std::vector<Node>,         /**< Sequence */
        std::map<std::string, Node>/**< Mapping */
    >;

    /** @brief Default: Null. */
    Node() noexcept : kind_(Kind::Null), value_(std::monostate{}) {}

    /** @brief Boolean. */       Node(bool b) noexcept : kind_(Kind::Bool),   value_(b) {}
    /** @brief Integer. */       Node(int64_t i) noexcept : kind_(Kind::Int),    value_(i) {}
    /** @brief Double. */        Node(double d) noexcept : kind_(Kind::Double), value_(d) {}
    /** @brief String. */        Node(std::string s) noexcept : kind_(Kind::String), value_(std::move(s)) {}
    /** @brief C-string. */      Node(const char* s)       : kind_(Kind::String), value_(std::string(s)) {}
    /** @brief Sequence. */      Node(std::vector<Node> seq) noexcept : kind_(Kind::Sequence), value_(std::move(seq)) {}
    /** @brief Mapping. */       Node(std::map<std::string, Node> map) noexcept : kind_(Kind::Mapping), value_(std::move(map)) {}

    /** @brief Node kind. */     Kind Type()    const noexcept { return kind_; }
    /** @brief Is Null. */       bool IsNull()  const noexcept { return kind_ == Kind::Null; }
    /** @brief Is scalar. */     bool IsScalar()const noexcept { return kind_ == Kind::Bool || kind_ == Kind::Int || kind_ == Kind::Double || kind_ == Kind::String; }
    /** @brief Is sequence. */   bool IsSequence() const noexcept { return kind_ == Kind::Sequence; }
    /** @brief Is mapping. */    bool IsMapping()  const noexcept { return kind_ == Kind::Mapping; }

    /** @brief As Bool. */       bool AsBool()   const { return std::get<bool>(AssertKind(Kind::Bool)); }
    /** @brief As Int. */        int64_t AsInt() const { return std::get<int64_t>(AssertKind(Kind::Int)); }
    /** @brief As Double. */     double AsDouble() const { return std::get<double>(AssertKind(Kind::Double)); }
    /** @brief As String. */     const std::string& AsString() const { return std::get<std::string>(AssertKind(Kind::String)); }
    /** @brief As Sequence. */   const std::vector<Node>& AsSequence() const { return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)); }
    /** @brief As Mapping. */    const std::map<std::string, Node>& AsMapping()  const { return std::get<std::map<std::string,Node>>(AssertKind(Kind::Mapping)); }

    /** @brief Mapping lookup (const). */
    const Node& operator[](const std::string& key) const {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping)).at(key);
    }
    /** @brief Mapping lookup (mutable). */
    Node& operator[](const std::string& key) {
        return std::get<std::map<std::string, Node>>(AssertKind(Kind::Mapping))[key];
    }
    /** @brief Sequence lookup (const). */
    const Node& operator[](size_t idx) const {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)).at(idx);
    }
    /** @brief Sequence lookup (mutable). */
    Node& operator[](size_t idx) {
        return std::get<std::vector<Node>>(AssertKind(Kind::Sequence)).at(idx);
    }

    /**
     * @brief Try to convert to T, or std::nullopt on mismatch.
     * @tparam T bool, int64_t, double, std::string
     */
    template<typename T>
    std::optional<T> TryAs() const;

private:
    /** @brief Ensure kind matches, return variant. */
    const Value& AssertKind(Kind expected) const {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " + KindToStr(expected) +
                                     ", got " + KindToStr(kind_) + ")");
        return value_;
    }
    /** @brief Mutable version. */
    Value& AssertKind(Kind expected) {
        if (kind_ != expected)
            throw std::runtime_error("YAML::Node: type mismatch (expected " + KindToStr(expected) +
                                     ", got " + KindToStr(kind_) + ")");
        return value_;
    }
    /** @brief Kind→string. */
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

    Kind   kind_;   /**< Node type. */
    Value  value_;  /**< Stored value. */
};

/// @cond INTERNAL
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
 * @brief Core YAML parser with block and flow style support.
 */
class Parser {
public:
    /**
     * @brief Parse YAML from string.
     * @param input YAML text.
     * @return Root node.
     * @throws std::runtime_error on error.
     */
    static Node ParseString(std::string_view input) {
        Parser p(input);
        return p.ParseDocument();
    }

private:
    /**
     * @struct LineInfo
     * @brief Preprocessed line: indent, content, line number.
     */
    struct LineInfo {
        size_t indent;            /**< Leading spaces. */
        std::string_view content; /**< Content after indent. */
        size_t line_no;           /**< 1-based. */
    };

    std::vector<LineInfo> lines_;               /**< All non-comment lines. */
    size_t cur_ = 0;                            /**< Current line index. */
    std::unordered_map<std::string, Node> anchors_; /**< Anchor table. */

    /**
     * @brief Constructor: split input into lines, skip comments/blanks.
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

    /** @brief Top-level document parse. */
    Node ParseDocument() {
        if (lines_.empty()) return Node();
        return ParseBlock(0);
    }

    /**
     * @brief Dispatch block vs flow vs sequence.
     * @param base_indent Indentation level.
     */
    Node ParseBlock(size_t base_indent) {
        auto& c = lines_[cur_].content;
        if (c.starts_with('-')) {
            return ParseSequence(base_indent);
        } else if (c.starts_with('[')) {
            auto close = c.find_last_of(']');
            if (close != std::string::npos)
                return ParseFlowSequence(c.substr(1, close - 1));
            throw std::runtime_error("YAML: Unterminated flow sequence");
        } else if (c.starts_with('{')) {
            auto close = c.find_last_of('}');
            if (close != std::string::npos)
                return ParseFlowMapping(c.substr(1, close - 1));
            throw std::runtime_error("YAML: Unterminated flow mapping");
        } else {
            return ParseMapping(base_indent);
        }
    }

    /**
     * @brief Extract `&anchor` if present.
     * @return {name, remainder}.
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
     * @brief Extract `*alias` if present.
     * @return {name,found}.
     */
    static std::pair<std::string,bool> parseAlias(std::string_view s) {
        auto pos = s.find('*');
        if (pos == std::string_view::npos) return {"",false};
        size_t i = pos+1;
        while (i<s.size() && (std::isalnum((unsigned char)s[i])||s[i]=='_'||s[i]=='-')) ++i;
        return {std::string(s.substr(pos+1, i-(pos+1))), true};
    }

    /**
     * @brief Parse a block-style mapping at indent.
     *        Supports anchors, aliases, merge keys, flow style in values.
     */
    Node ParseMapping(size_t base_indent) {
        std::map<std::string,Node> m;
        while (cur_ < lines_.size() && lines_[cur_].indent >= base_indent) {
            auto& li = lines_[cur_];
            auto pos = li.content.find(':');
            if (pos == std::string::npos)
                throw std::runtime_error("YAML: missing ':' at line "+std::to_string(li.line_no));
            // key + key-alias
            auto keyv = Trim(li.content.substr(0,pos));
            auto [kanchor, kbase] = parseAnchor(keyv);
            std::string key(kbase);
            // rest + value-anchor
            auto rest = li.content.substr(pos+1);
            size_t sp=0; while(sp<rest.size()&&rest[sp]==' ')++sp;
            rest = rest.substr(sp);
            auto [vanchor, vrem] = parseAnchor(rest);
            std::string val_anchor = vanchor;
            std::string_view content = vrem;

            Node val;
            // multi-line scalar
            if (content == "|" || content == ">") {
                char t = content[0]; ++cur_;
                val = ParseMultilineScalar(li.indent+2, t);
            }
            // alias/merge
            else if (auto [a, ia] = parseAlias(content); ia) {
                if (!anchors_.count(a))
                    throw std::runtime_error("YAML: undefined alias '*"+a+"' at line "+std::to_string(li.line_no));
                val = anchors_[a];
                if (key=="<<") {
                    if (!val.IsMapping())
                        throw std::runtime_error("YAML: merge key '*"+a+"' not mapping");
                    for(auto& kv: val.AsMapping()) m.insert(kv);
                    ++cur_; continue;
                }
                ++cur_;
            }
            // flow-style mapping value
            else if (content.starts_with('{')) {
                val = ParseFlowMapping(content.substr(1, content.size()-2));
                ++cur_;
            }
            // flow-style sequence value
            else if (content.starts_with('[')) {
                val = ParseFlowSequence(content.substr(1, content.size()-2));
                ++cur_;
            }
            // nested block or null
            else if (content.empty()) {
                ++cur_;
                if (cur_<lines_.size() && lines_[cur_].indent>li.indent)
                    val = ParseBlock(li.indent+2);
                else
                    val = Node();
            }
            // plain scalar
            else {
                ++cur_;
                val = ParseScalar(content);
            }

            // register anchors
            if (!kanchor.empty())   anchors_[kanchor]   = val;
            if (!val_anchor.empty()) anchors_[val_anchor] = val;
            m.emplace(std::move(key), std::move(val));
        }
        return Node(std::move(m));
    }

    /**
     * @brief Parse a block-style sequence at indent.
     *        Supports anchors, aliases, nested blocks, and flow style elements.
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
            if (content.empty()) {
                ++cur_;
                elt = ParseBlock(li.indent+2);
            }
            else if (content == "|" || content == ">") {
                char t = content[0]; ++cur_;
                elt = ParseMultilineScalar(li.indent+2, t);
            }
            else if (auto [a, ia] = parseAlias(content); ia) {
                if (!anchors_.count(a))
                    throw std::runtime_error("YAML: undefined alias '*"+a+"' in sequence");
                elt = anchors_[a];
                ++cur_;
            }
            else if (content.starts_with('{')) {
                elt = ParseFlowMapping(content.substr(1, content.size()-2));
                ++cur_;
            }
            else if (content.starts_with('[')) {
                elt = ParseFlowSequence(content.substr(1, content.size()-2));
                ++cur_;
            }
            else {
                ++cur_;
                elt = ParseScalar(content);
            }

            if (!elt_anchor.empty()) anchors_[elt_anchor] = elt;
            seq.push_back(std::move(elt));
        }
        return Node(std::move(seq));
    }

    /**
     * @brief Parse a multi-line scalar ('|' literal or '>' folded).
     * @param indent Required indentation.
     * @param type '|' or '>'.
     * @return Scalar node containing the full text.
     */
    Node ParseMultilineScalar(size_t indent, char type) {
        std::string out;
        bool first = true;
        while (cur_ < lines_.size() && lines_[cur_].indent >= indent) {
            auto line = lines_[cur_].content;
            if (!first) {
                if (type=='|') out.push_back('\n');
                else            out.push_back(line.empty()?'\n':' ');
            }
            out += line;
            first = false;
            ++cur_;
        }
        return Node(std::move(out));
    }

    /**
     * @brief Parse a single-line scalar (null, bool, int, double, quoted, or plain).
     * @param s Input string_view.
     * @return Scalar node.
     */
    static Node ParseScalar(std::string_view s) {
        if (s=="~"||s=="null"||s=="Null"||s=="NULL")     return Node();
        if (s=="true"||s=="True"||s=="TRUE")             return Node(true);
        if (s=="false"||s=="False"||s=="FALSE")          return Node(false);
        int64_t i=0;
        if (auto [p,ec] = std::from_chars(s.data(),s.data()+s.size(),i);
            ec==std::errc() && p==s.data()+s.size())
            return Node(i);
        double d=0;
        if (auto [p2,ec2] = std::from_chars(s.data(),s.data()+s.size(),d);
            ec2==std::errc() && p2==s.data()+s.size())
            return Node(d);
        if ((s.starts_with('"') && s.ends_with('"')) ||
            (s.starts_with('\'') && s.ends_with('\'')))
            return Node(std::string(s.substr(1,s.size()-2)));
        return Node(std::string(s));
    }

    /**
     * @brief Trim whitespace from both ends.
     * @param s Input view.
     * @return Trimmed view.
     */
    static std::string_view Trim(std::string_view s) {
        size_t b=0, e=s.size();
        while (b<e && std::isspace((unsigned char)s[b])) ++b;
        while (e>b && std::isspace((unsigned char)s[e-1])) --e;
        return s.substr(b, e-b);
    }

    /**
     * @brief Parse a Flow-style sequence ([a,b,c]).
     * @param s Contents inside brackets.
     * @return Sequence node.
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
     * @brief Parse a Flow-style mapping ({a:1,b:2}).
     * @param s Contents inside braces.
     * @return Mapping node.
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

inline Node ParseString(std::string_view in) {
    return Parser::ParseString(in);
}

inline Node ParseFile(const std::string& fn) {
    std::ifstream f(fn);
    if (!f) throw std::runtime_error("YAML: cannot open file: "+fn);
    std::stringstream ss; ss<<f.rdbuf();
    return ParseString(ss.str());
}

} // namespace YAML

#endif // YAML_BUTTER_HPP
