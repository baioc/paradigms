#include <iostream>
#include <fstream>  // filestream
#include <sstream>  // stringstream
#include <stack>
#include <string>
#include <string.h>


bool balanced(const std::string& xml);

int main(int argc, char const *argv[])
{
	if (argc < 2) {
		std::cout << "chkxml:\n"
		          << "  Checks if given XMLs' tag structures are valid.\n"
		          << "  Returns the number of files found to be incorrect, echo $? to see this value.\n"
		          << "\nUsage:\n"
		          << "  " << argv[0] << " file.xml...\n"
		          << "  " << argv[0] << " < file.xml\n"
		          << "  cat file.xml | " << argv[0] << " - [file.xml...]\n";
		return -1;
	}

	int err = 0;
	for (int i = 1; i < argc; ++i) {
		std::stringstream buffer;

		// if actual filename
		if (strcmp(argv[i], "-") != 0) {
			std::ifstream file{argv[i]};

			if (!file.is_open()) {
				std::cout << "Could not open " << argv[i] << '\n';
				++err;
				continue;
			}

			buffer << file.rdbuf();
			file.close();
		}
		// otherwise use stdin
		else {
			buffer << std::cin.rdbuf();
		}

		const bool ok = balanced(buffer.str());
		err += !ok;
		std::cout << (strcmp(argv[i], "-") != 0 ? argv[i] : "Input")
		          << (ok ? " seems OK." : " is NOT correct.")
		          << '\n';
	}

	return err;
}

bool balanced(const std::string& xml)
{
	std::stack<std::string> tags;

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
			tags.push(tag.insert(1, "/"));
		}
		// otherwise (if its a closing tag), check if it was expected
		else {
			if (tags.empty())
				return false;
			else if (tags.top() == tag)
				tags.pop();
			else
				return false;
		}
	}

	// all tags must have been closed
	return tags.empty();
}
