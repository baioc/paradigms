#include "trie.hpp"

#include <iostream>
#include <string>

int main()
{
	using namespace std;
	using namespace structures;

	// string filename;
	// string word;

	// cin >> filename;
	// cout << filename << endl; // esta linha deve ser removida

	// while (true) { // leitura das palavras ate encontrar "0"
	// 	cin >> word;
	// 	if (word == "0")
	// 		break;
	// 	cout << word << endl;
	// }

	Trie<string> trie;

	cout << trie.empty() << endl;
	cout << trie.size() << endl;

	trie.put("batata", "alimento");
	trie.put("batatata", "alimentoento");
	trie.put("banana", "alimento, fruta");
	trie.put("batman", "heroi, morcego");

	cout << trie << endl;

	cout << trie.empty() << endl;
	cout << trie.size() << endl;

	cout << trie.contains("") << endl;
	cout << trie.contains("batata") << endl;
	cout << trie.contains("batatata") << endl;
	cout << trie.contains("banana") << endl;
	cout << trie.contains("batman") << endl;

	cout << *trie.get("batata") << endl;
	trie.remove("batata");
	cout << *trie.get("batatata") << endl;
	cout << *trie.get("banana") << endl;
	cout << *trie.get("batman") << endl;

	trie.remove("batatata");
	trie.remove("banana");
	trie.remove("batman");

	cout << trie.contains("batata") << endl;
	cout << trie.contains("batatata") << endl;
	cout << trie.contains("banana") << endl;
	cout << trie.contains("batman") << endl;

	cout << trie.empty() << endl;
	cout << trie.size() << endl;
}
