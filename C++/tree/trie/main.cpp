#include "trie.hpp"

#include <iostream>
#include <string>
#include <fstream>

int main()
{
	using namespace std;
	using namespace structures;

	Trie<pair<size_t,size_t>> dict;

	string filename;
	cin >> filename;

	ifstream file{filename};
	if (!file.is_open()) return 1;

	size_t cursor = 0;
	for (string line; getline(file, line); cursor += line.length() + 1) {
		const auto open_pos = line.find('[', 0);
		const auto close_pos = line.find(']', open_pos);
		if (open_pos == string::npos || close_pos == string::npos) continue;
		const auto word = line.substr(open_pos + 1, close_pos - open_pos - 1);
		dict.put(word, {cursor, line.length()});
	}

	file.close();

	for (string word; word != "0"; ) {
		cin >> word;
		if (word == "0") {
			break;
		} else if (dict.contains(word)) {
			const auto idx = dict.get(word);
			cout << idx->first << ' ' << idx->second << endl;
		} else {
			cout << (dict.keys(word).empty() ? "is not prefix" : "is prefix") << endl;
		}
	}
}
