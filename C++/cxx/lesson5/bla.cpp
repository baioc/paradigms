bool TGAImage::read_tga_file(const char *filename) {
	if (data) delete [] data;
	data = NULL;
	std::ifstream in;
	in.open (filename, std::ios::binary);
	if (!in.is_open()) {
		std::cerr << "can't open file " << filename << "\n";
		in.close();
		return false;
	}
	TGA_Header header;
	in.read((char *)&header, sizeof(header));
	if (!in.good()) {
		in.close();
		std::cerr << "an error occured while reading the header\n";
		return false;
	}
	width   = header.width;
	height  = header.height;
	bytespp = header.bitsperpixel>>3;
	if (width<=0 || height<=0 || (bytespp!=GRAYSCALE && bytespp!=RGB && bytespp!=RGBA)) {
		in.close();
		std::cerr << "bad bpp (or width/height) value\n";
		return false;
	}
	unsigned long nbytes = bytespp*width*height;
	data = new unsigned char[nbytes];
	if (3==header.datatypecode || 2==header.datatypecode) {
		in.read((char *)data, nbytes);
		if (!in.good()) {
			in.close();
			std::cerr << "an error occured while reading the data\n";
			return false;
		}
	} else if (10==header.datatypecode||11==header.datatypecode) {
		if (!load_rle_data(in)) {
			in.close();
			std::cerr << "an error occured while reading the data\n";
			return false;
		}
	} else {
		in.close();
		std::cerr << "unknown file format " << (int)header.datatypecode << "\n";
		return false;
	}
	if (!(header.imagedescriptor & 0x20)) {
		flip_vertically();
	}
	if (header.imagedescriptor & 0x10) {
		flip_horizontally();
	}
	std::cerr << width << "x" << height << "/" << bytespp*8 << "\n";
	in.close();
	return true;
}


bool TGAImage::write_tga_file(const char *filename, bool rle) {
	unsigned char developer_area_ref[4] = {0, 0, 0, 0};
	unsigned char extension_area_ref[4] = {0, 0, 0, 0};
	unsigned char footer[18] = {'T','R','U','E','V','I','S','I','O','N','-','X','F','I','L','E','.','\0'};
	std::ofstream out;
	out.open (filename, std::ios::binary);
	if (!out.is_open()) {
		std::cerr << "can't open file " << filename << "\n";
		return false;
	}
	TGA_Header header;
	memset((void *)&header, 0, sizeof(header));
	header.bitsperpixel = bytespp<<3;
	header.width  = width;
	header.height = height;
	header.datatypecode = (bytespp==GRAYSCALE?(rle?11:3):(rle?10:2));
	header.imagedescriptor = 0x20; // top-left origin
	out.write((char *)&header, sizeof(header));
	if (!out.good()) {
		std::cerr << "can't dump the tga file\n";
		return false;
	}
	if (!rle) {
		out.write((char *)data, width*height*bytespp);
		if (!out.good()) {
			std::cerr << "can't unload raw data\n";
			return false;
		}
	} else {
		if (!unload_rle_data(out)) {
			std::cerr << "can't unload rle data\n";
			return false;
		}
	}
	out.write((char *)developer_area_ref, sizeof(developer_area_ref));
	if (!out.good()) {
		std::cerr << "can't dump the tga file\n";
		return false;
	}
	out.write((char *)extension_area_ref, sizeof(extension_area_ref));
	if (!out.good()) {
		std::cerr << "can't dump the tga file\n";
		return false;
	}
	out.write((char *)footer, sizeof(footer));
	if (!out.good()) {
		std::cerr << "can't dump the tga file\n";
		return false;
	}
	return true;
}
