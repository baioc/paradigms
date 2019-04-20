#include <iostream>
#include <fstream>	// filestream
#include <sstream>	// stringstream
#include <vector>
#include <string>


bool balanced(const std::string& xml);

int main(int argc, char const *argv[])
{
	if (argc < 2) {
		std::cout << "Usage: " << argv[0] << " [file.xml]...\n";
		return -1;
	}

	int err = 0;
	for (int i = 1; i < argc; ++i) {
		std::ifstream xml(argv[i]);
		if (!xml.is_open()) {
			std::cout << "Could not open " << argv[i] << '\n';
			++err;
			continue;
		}

		std::stringstream buffer;
		buffer << xml.rdbuf();

		const bool ok = balanced(buffer.str());
		err += !ok;
		std::cout << argv[i]
		          << (ok ? " seems OK." : " is NOT correct.")
		          << '\n';
	}

	return err;
}

bool balanced(const std::string& xml)
{
	std::vector<std::string> tags;

	auto from = 0u;
	while (from < xml.length()) {
		// check for tag
		const auto tag_open = xml.find('<', from);
		const auto tag_close = xml.find('>', tag_open);

		// no tag found
		if (tag_open == std::string::npos)
			break;

		// incomplete tag
		if (tag_close == std::string::npos)
			return false;

		// get tag
		auto tag = xml.substr(tag_open, tag_close + 1 - tag_open);
		from = tag_close + 1;

		// if its an opening tag, push the closing one
		if (tag[1] != '/') {
			tags.push_back(tag.insert(1, "/"));
		} // if its a closing tag, check if it was expected
		else {
			if (tags.empty())
				return false;
			else if (tags.back() == tag)
				tags.pop_back();
			else
				return false;
		}
	}

	// all tags must have been closed
	return tags.empty();
}
