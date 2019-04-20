#include "array_queue.hpp"

#include <iostream>
#include <cstdlib>


using baioc::Queue;

template <typename T>
void dump(Queue<T>& q) {
	std::cout << "[";
	int max = q.size();
	for (int i = 0; i < max; ++i) {
		auto aux = q.dequeue();
		std::cout << (i == 0 ? "" : " ")
		          << aux;
		q.enqueue(aux);
	}
	std::cout << "]\n";
}

template <typename T>
int user_command(Queue<T>& q) {
	std::cout << "Comandos:\n"
	          << "r: retirar\n"
			  << "c: colocar\n"
			  << "q: sair\n\n"
			  << "> ";

    char cmd;
	std::cin >> cmd;

	switch (cmd) {
		case 'q':
			return 1;
			break;

		case 'r':
			try {
				std::cout << "--> " << q.dequeue() << '\n';
			} catch(const std::exception& e) {
				std::cout << e.what() << '\n';
			}
			dump(q);
			break;

		case 'c':
			{
				auto r = std::rand() % 1000;
				std::cout << "<-- " << r << '\n';
				try {
					q.enqueue(r);
				} catch(const std::exception& e) {
					std::cout << e.what() << '\n';
				}
			}
			dump(q);
			break;

		default:
			return -1;
			break;
	}

	return 0;
}


int main(int argc, char const *argv[]) {
	int capacity = 0;
    std::cout << "Digite o tamanho do buffer:\n> ";
	std::cin >> capacity;

	Queue<int> buffer(capacity);

    while (!user_command(buffer));

    return 0;
}
