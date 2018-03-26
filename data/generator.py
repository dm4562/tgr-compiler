import json

GRAMMAR_FILE = 'grammar.txt'
JSON_FILE = 'grammar.json'

def generate_grammar():
    count = 1
    seen = {}
    non_terminals = [""]
    productions = [""]
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
        "nonterminals": non_terminals
    }

    with open(JSON_FILE, 'w') as out:
        json.dump(obj, out)


def main():
    generate_grammar()

if __name__ == '__main__':
    main()