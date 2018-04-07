import json
import csv
from pprint import pprint
import copy

GRAMMAR_FILE = 'grammar.txt'
GRAMMR_JSON_FILE = 'grammar.json'
PARSE_TABLE_JSON_FILE = 'parsing_table.json'
TEMP_FILE = 'temp_grammar.txt'
EPSILON = "EPSILON"

def generate_grammar():
    count = 1
    seen = {}
    non_terminals = [""]
    productions = [""]
    rules = [""]
    with open(GRAMMAR_FILE) as f:
        for line in f:
            line = line.strip()
            if line.startswith("=====") or line == "":
                break
            line = line.split("->")
            nt = line[0].strip()
            prod = line[1].strip()
            if nt not in seen:
                seen[nt] = str(count)
                count += 1
                non_terminals.append(nt)
            productions.append(prod)
            rules.append(nt)

    final_productions = []
    for prod in productions:
        prod = prod.split(" ")
        out = []
        for e in prod:
            if e in seen:
                out.append(seen[e])
            elif e != "EPSILON":
                out.append(e)
        final_productions.append(out)

    obj = {
        "productions": final_productions,
        "nonterminals": non_terminals,
        "rules": rules
    }

    with open(GRAMMR_JSON_FILE, 'w') as out:
        json.dump(obj, out)

def generate_parse_table():
    first = generate_first_sets()
    follow = generate_follow_sets()

    terminal_map = {}
    terminals = [""]
    count = 1

    grammar = json.load(open(GRAMMR_JSON_FILE))

    for prod in grammar['productions']:
        for p in prod:
            try:
                nt = int(p)
            except:
                if p not in terminal_map:
                    terminal_map[p] = count
                    count += 1
                    terminals.append(p)

        if not prod:
            prod.append(EPSILON)

    terminal_map['$'] = count
    count += 1
    terminals.append('$')
    print(count)

    nonterminal_map = {}
    for i, nt in enumerate(grammar['nonterminals']):
        nonterminal_map[nt] = i

    parse_table = [[len(grammar['rules']) for _ in terminals] for _ in grammar['nonterminals']]
    print(len(parse_table), len(parse_table[0]))

    def get_prod(production, i):
        try:
            p0 = int(prod[i])
            p0 = grammar['nonterminals'][p0]
        except:
            p0 = prod[i]
        return p0

    for i in range(len(grammar['nonterminals'])):
        parse_table[i][0] = 0

    for i in range(len(terminals)):
        parse_table[0][i] = 0

    for i in range(1, len(grammar['rules'])):
        r = grammar['rules'][i]
        prod = grammar['productions'][i]
        p = get_prod(prod, 0)

        first_plus = first[p].copy()

        for j in range(1, len(prod)):
            p = get_prod(prod, j)

            if EPSILON in first_plus:
                first_plus.discard(EPSILON)
                first_plus |= first[p]

        if EPSILON in first_plus:
            first_plus.discard(EPSILON)
            first_plus |= follow[r]

        ntix = nonterminal_map[r]
        for terminal in first_plus:
            tix = terminal_map[terminal]
            if parse_table[ntix][tix] != len(grammar['rules']):
                print(i, terminal, tix, r, ntix)
                print(parse_table[nonterminal_map[r]][tix])
                raise ValueError("Table entry clash")
            else:
                parse_table[nonterminal_map[r]][terminal_map[terminal]] = i

    parse_obj = {
        'terminals': terminals,
        'table': parse_table
    }

    with open(PARSE_TABLE_JSON_FILE, 'w') as f:
        json.dump(parse_obj, f)

def generate_follow_sets():
    first = generate_first_sets()
    grammar = json.load(open(GRAMMR_JSON_FILE))
    follow = {}
    for nt in grammar['nonterminals']:
        follow[nt] = set()

    follow['PROGRAM'].add('$')
    pre = None

    while pre != follow:
        pre = copy.deepcopy(follow)

        for r, prod in zip(grammar['rules'], grammar['productions']):
            trailer = follow[r].copy()
            j = len(prod) - 1
            while j >= 0:
                try:
                    p = int(prod[j])
                    p = grammar['nonterminals'][p]
                    is_nt = True
                except:
                    p = prod[j]
                    is_nt = False

                if is_nt:
                    follow[p] |= trailer

                    if EPSILON in first[p]:
                        trailer |= (first[p] - set([EPSILON]))
                    else:
                        trailer = first[p]
                else:
                    trailer = set([p])

                j -= 1

    return follow

def generate_first_sets():
    grammar = json.load(open(GRAMMR_JSON_FILE))
    first = {}
    for i, prod in enumerate(grammar['productions']):
        if not prod:
            prod.append(EPSILON)

        for p in prod:
            try:
                p = int(p)
            except:
                first[p] = set([p])

    for nt in grammar['rules']:
        first[nt] = set()

    pre = copy.deepcopy(first)
    assert(pre == first)
    pre = None

    def get_prod(production, i):
        try:
            p0 = int(prod[i])
            p0 = grammar['nonterminals'][p0]
        except:
            p0 = prod[i]
        return p0

    count = 0
    while pre != first:
        pre = copy.deepcopy(first)
        count += 1
        for r, prod in zip(grammar['rules'], grammar['productions']):
            p0 = get_prod(prod, 0)
            rhs = first[p0].copy() - set([EPSILON])

            j = 0
            while j < len(prod) - 1:
                pi = get_prod(prod, j)
                pi_1 = get_prod(prod, j + 1)

                if EPSILON not in first[pi]:
                    break

                rhs |= first[pi_1]
                j += 1

            pi = get_prod(prod, j)
            if j == len(prod) - 1 and EPSILON in first[pi]:
                rhs.add(EPSILON)

            first[r] |= rhs

    return first

def main():
    generate_grammar()
    generate_parse_table()

if __name__ == '__main__':
    main()