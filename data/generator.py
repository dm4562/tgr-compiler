import json
import csv

GRAMMAR_FILE = 'grammar.txt'
JSON_FILE = 'grammar.json'
TEMP_FILE = 'temp_grammar.txt'

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

    with open(TEMP_FILE, 'w') as temp:
        with open(GRAMMAR_FILE) as file:
            for line in file:
                line = line.strip()
                if line.startswith("=====") or line == "":
                    break
                line = line.split("->")
                prod = line[1].strip()
                prod = prod.split(" ")
                out = []
                for p in prod:
                    if not p.isupper():
                        out.append("\"{}\"".format(p))
                    else:
                        out.append(p)

                prod = " ".join(out)
                temp.write("{} : {} ;\n".format(line[0].strip(), prod))

            temp.write("EPSILON : ;\n")

def convert_ept_parse_table():
    nonterminals = []
    table = []
    with open("ept.csv") as file:
        reader = csv.reader(file)
        first = next(reader)
        # print(first)
        for e in first[:-1]:
            if e == 0:
                continue

            e = e.replace("\"", "")
            nonterminals.append(e)

        table.append([0 for _ in nonterminals])
        # for row in reader:
        #     # print(row)
        #     jrow = [0]
        #     for e in row[2:-1]:
        #         if e

        print(nonterminals)

def main():
    generate_grammar()
    # convert_ept_parse_table()

if __name__ == '__main__':
    main()