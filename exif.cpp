/**************************************************************************
  exif.cpp  -- A simple ISO C++ library to parse basic EXIF
               information from a JPEG file.

  Copyright (c) 2010-2015 Mayank Lahiri
  mlahiri@gmail.com
  All rights reserved (BSD License).

  See exif.h for version history.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  -- Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
  -- Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY EXPRESS
  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
  NO EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "exif.h"

#include <algorithm>
#include <cstdint>
#include <stdio.h>
#include <vector>
#include <string.h> //qlh

using std::string;

namespace {
#define UCHAR unsigned char
#define SATIR
#define EXIF_FILE_SIZE                          28800
#define MAX_JPG_THUMBNAIL_WIDTH         320
#define MAX_JPG_THUMBNAIL_HEIGHT        240
#define MAX_FILE_THUMB_SIZE                     (MAX_JPG_THUMBNAIL_WIDTH * MAX_JPG_THUMBNAIL_HEIGHT)

static const int APP1HeaderLen = 0x0C;
static UCHAR APP1Marker[] = {0xFF, 0xE1};
static UCHAR SATTag[] = {'S', 'A', 'T', 'I', 'R', 0x0};
static UCHAR MAX_APP1_LEN[] = {0xFF, 0xFE};
static const int MAX_DATA_LEN = 0xFFF4;

UCHAR ExifHeader[6]=
{
        0x45,0x78,0x69,0x66,0x00,0x00
};

UCHAR TIFFHeader[8]=
{
        0x49,0x49,0x2A,0x00,0x08,0x00,0x00,0x00
};

struct Rational {
  uint32_t numerator, denominator;
  operator double() const {
    if (denominator < 1e-20) {
      return 0;
    }
    return static_cast<double>(numerator) / static_cast<double>(denominator);
  }
};

// IF Entry
class IFEntry {
 public:
  using byte_vector = std::vector<uint8_t>;
  using ascii_vector = std::string;
  using short_vector = std::vector<uint16_t>;
  using long_vector = std::vector<uint32_t>;
  using rational_vector = std::vector<Rational>;

  IFEntry()
      : tag_(0xFF), format_(0xFF), data_(0), length_(0), val_byte_(nullptr) {}
  IFEntry(const IFEntry &) = delete;
  IFEntry &operator=(const IFEntry &) = delete;
  IFEntry(IFEntry &&other)
      : tag_(other.tag_),
        format_(other.format_),
        data_(other.data_),
        length_(other.length_),
        val_byte_(other.val_byte_) {
    other.tag_ = 0xFF;
    other.format_ = 0xFF;
    other.data_ = 0;
    other.length_ = 0;
    other.val_byte_ = nullptr;
  }
  ~IFEntry() { delete_union(); }
  unsigned short tag() const { return tag_; }
  void tag(unsigned short tag) { tag_ = tag; }
  unsigned short format() const { return format_; }
  bool format(unsigned short format) {
    switch (format) {
      case 0x01:
      case 0x02:
      case 0x03:
      case 0x04:
      case 0x05:
      case 0x07: // qlh UserComment data format ID
      case 0x09:
      case 0x0a:
      case 0xff:
        break;
      default:
        return false;
    }
    delete_union();
    format_ = format;
    new_union();
    return true;
  }
  unsigned data() const { return data_; }
  void data(unsigned data) { data_ = data; }
  unsigned length() const { return length_; }
  void length(unsigned length) { length_ = length; }

  // functions to access the data
  //
  // !! it's CALLER responsibility to check that format !!
  // !! is correct before accessing it's field          !!
  //
  // - getters are use here to allow future addition
  //   of checks if format is correct
  byte_vector &val_byte() { return *val_byte_; }
  ascii_vector &val_string() { return *val_string_; }
  short_vector &val_short() { return *val_short_; }
  long_vector &val_long() { return *val_long_; }
  rational_vector &val_rational() { return *val_rational_; }

 private:
  // Raw fields
  unsigned short tag_;
  unsigned short format_;
  unsigned data_;
  unsigned length_;

  // Parsed fields
  union {
    byte_vector *val_byte_;
    ascii_vector *val_string_;
    short_vector *val_short_;
    long_vector *val_long_;
    rational_vector *val_rational_;
  };

  void delete_union() {
    switch (format_) {
      case 0x1:
        delete val_byte_;
        val_byte_ = nullptr;
        break;
      case 0x2:
	  case 0x7: //qlh 0x7 0x2
        delete val_string_;
        val_string_ = nullptr;
        break;
      case 0x3:
        delete val_short_;
        val_short_ = nullptr;
        break;
      case 0x4:
        delete val_long_;
        val_long_ = nullptr;
        break;
      case 0x5:
        delete val_rational_;
        val_rational_ = nullptr;
        break;
      case 0xff:
        break;
      default:
        // should not get here
        // should I throw an exception or ...?
        break;
    }
  }
  void new_union() {
    switch (format_) {
      case 0x1:
        val_byte_ = new byte_vector();
        break;
      case 0x2:
	  case 0x7: //qlh  0x7 使用与0x2一样的数据格式
        val_string_ = new ascii_vector();
        break;
      case 0x3:
        val_short_ = new short_vector();
        break;
      case 0x4:
        val_long_ = new long_vector();
        break;
      case 0x5:
        val_rational_ = new rational_vector();
        break;
      case 0xff:
        break;
      default:
        // should not get here
        // should I throw an exception or ...?
        break;
    }
  }
};

// Helper functions
template <typename T, bool alignIntel>
T parse(const unsigned char *buf);

template <>
uint8_t parse<uint8_t, false>(const unsigned char *buf) {
  return *buf;
}

template <>
uint8_t parse<uint8_t, true>(const unsigned char *buf) {
  return *buf;
}

template <>
uint16_t parse<uint16_t, false>(const unsigned char *buf) {
  return (static_cast<uint16_t>(buf[0]) << 8) | buf[1];
}

template <>
uint16_t parse<uint16_t, true>(const unsigned char *buf) {
  return (static_cast<uint16_t>(buf[1]) << 8) | buf[0];
}

template <>
uint32_t parse<uint32_t, false>(const unsigned char *buf) {
  return (static_cast<uint32_t>(buf[0]) << 24) |
         (static_cast<uint32_t>(buf[1]) << 16) |
         (static_cast<uint32_t>(buf[2]) << 8) | buf[3];
}

template <>
uint32_t parse<uint32_t, true>(const unsigned char *buf) {
  return (static_cast<uint32_t>(buf[3]) << 24) |
         (static_cast<uint32_t>(buf[2]) << 16) |
         (static_cast<uint32_t>(buf[1]) << 8) | buf[0];
}

template <>
Rational parse<Rational, true>(const unsigned char *buf) {
  Rational r;
  r.numerator = parse<uint32_t, true>(buf);
  r.denominator = parse<uint32_t, true>(buf + 4);
  return r;
}

template <>
Rational parse<Rational, false>(const unsigned char *buf) {
  Rational r;
  r.numerator = parse<uint32_t, false>(buf);
  r.denominator = parse<uint32_t, false>(buf + 4);
  return r;
}

/**
 * Try to read entry.length() values for this entry.
 *
 * Returns:
 *  true  - entry.length() values were read
 *  false - something went wrong, vec's content was not touched
 */
template <typename T, bool alignIntel, typename C>
bool extract_values(C &container, const unsigned char *buf, const unsigned base,
                    const unsigned len, const IFEntry &entry) {
  const unsigned char *data;
  uint32_t reversed_data;
  // if data fits into 4 bytes, they are stored directly in
  // the data field in IFEntry
  if (sizeof(T) * entry.length() <= 4) {
    if (alignIntel) {
      reversed_data = entry.data();
    } else {
      reversed_data = entry.data();
      // this reversing works, but is ugly
      unsigned char *rdata = reinterpret_cast<unsigned char *>(&reversed_data);
      unsigned char tmp;
      tmp = rdata[0];
      rdata[0] = rdata[3];
      rdata[3] = tmp;
      tmp = rdata[1];
      rdata[1] = rdata[2];
      rdata[2] = tmp;
    }
    data = reinterpret_cast<const unsigned char *>(&(reversed_data));
  } else {
    data = buf + base + entry.data();
    if (data + sizeof(T) * entry.length() > buf + len) {
      return false;
    }
  }
  container.resize(entry.length());
  
  //qlh 对于UserComment(0x7)的data区,前8个字节描述的是字符集
  //一般为ASCII字符集,为{0x41,0x53,0x43,0x49,0x49,0x00,0x00,0x00}
  size_t i = 0;
  size_t offset = 0;
  if(entry.format() == 0x7){
	i = 8;
  }
  for (; i < entry.length(); ++i) {
    container[offset++] = parse<T, alignIntel>(data + sizeof(T) * i);
  }
  return true;
}

template <bool alignIntel>
void parseIFEntryHeader(const unsigned char *buf, unsigned short &tag,
                        unsigned short &format, unsigned &length,
                        unsigned &data) {
  // Each directory entry is composed of:
  // 2 bytes: tag number (data field)
  // 2 bytes: data format
  // 4 bytes: number of components
  // 4 bytes: data value or offset to data value
  tag = parse<uint16_t, alignIntel>(buf);
  format = parse<uint16_t, alignIntel>(buf + 2);
  length = parse<uint32_t, alignIntel>(buf + 4);
  data = parse<uint32_t, alignIntel>(buf + 8);
}

template <bool alignIntel>
void parseIFEntryHeader(const unsigned char *buf, IFEntry &result) {
  unsigned short tag;
  unsigned short format;
  unsigned length;
  unsigned data;

  parseIFEntryHeader<alignIntel>(buf, tag, format, length, data);

  result.tag(tag);
  result.format(format);
  result.length(length);
  result.data(data);
}

template <bool alignIntel>
IFEntry parseIFEntry_temp(const unsigned char *buf, const unsigned offs,
                          const unsigned base, const unsigned len) {
  IFEntry result;

  // check if there even is enough data for IFEntry in the buffer
  if (buf + offs + 12 > buf + len) {
    result.tag(0xFF);
    return result;
  }

  parseIFEntryHeader<alignIntel>(buf + offs, result);

  // Parse value in specified format
  switch (result.format()) {
    case 1:
      if (!extract_values<uint8_t, alignIntel>(result.val_byte(), buf, base,
                                               len, result)) {
        result.tag(0xFF);
      }
      break;
    case 2:
	case 7:
      // string is basically sequence of uint8_t (well, according to EXIF even
      // uint7_t, but
      // we don't have that), so just read it as bytes
      if (!extract_values<uint8_t, alignIntel>(result.val_string(), buf, base,
                                               len, result)) {
        result.tag(0xFF);
      }
      // and cut zero byte at the end, since we don't want that in the
      // std::string
      if (result.val_string()[result.val_string().length() - 1] == '\0') {
        result.val_string().resize(result.val_string().length() - 1);
      }
      break;
    case 3:
      if (!extract_values<uint16_t, alignIntel>(result.val_short(), buf, base,
                                                len, result)) {
        result.tag(0xFF);
      }
      break;
    case 4:
      if (!extract_values<uint32_t, alignIntel>(result.val_long(), buf, base,
                                                len, result)) {
        result.tag(0xFF);
      }
      break;
    case 5:
      if (!extract_values<Rational, alignIntel>(result.val_rational(), buf,
                                                base, len, result)) {
        result.tag(0xFF);
      }
      break;
    case 9:
    case 10:
      break;
    default:
      result.tag(0xFF);
  }
  return result;
}

// helper functions for convinience
template <typename T>
T parse_value(const unsigned char *buf, bool alignIntel) {
  if (alignIntel) {
    return parse<T, true>(buf);
  } else {
    return parse<T, false>(buf);
  }
}

void parseIFEntryHeader(const unsigned char *buf, bool alignIntel,
                        unsigned short &tag, unsigned short &format,
                        unsigned &length, unsigned &data) {
  if (alignIntel) {
    parseIFEntryHeader<true>(buf, tag, format, length, data);
  } else {
    parseIFEntryHeader<false>(buf, tag, format, length, data);
  }
}

IFEntry parseIFEntry(const unsigned char *buf, const unsigned offs,
                     const bool alignIntel, const unsigned base,
                     const unsigned len) {
  if (alignIntel) {
    return parseIFEntry_temp<true>(buf, offs, base, len);
  } else {
    return parseIFEntry_temp<false>(buf, offs, base, len);
  }
}
}

//
// Locates the EXIF segment and parses it using parseFromEXIFSegment
//
int easyexif::EXIFInfo::parseFrom(const unsigned char *buf, unsigned len) {
  // Sanity check: all JPEG files start with 0xFFD8.
  if (!buf || len < 4) return PARSE_EXIF_ERROR_NO_JPEG;
  if (buf[0] != 0xFF || buf[1] != 0xD8) return PARSE_EXIF_ERROR_NO_JPEG;

  // Sanity check: some cameras pad the JPEG image with some bytes at the end.
  // Normally, we should be able to find the JPEG end marker 0xFFD9 at the end
  // of the image buffer, but not always. As long as there are some bytes
  // except 0xD9 at the end of the image buffer, keep decrementing len until
  // an 0xFFD9 is found. If JPEG end marker 0xFFD9 is not found,
  // then we can be reasonably sure that the buffer is not a JPEG.
  while (len > 2) {
    if (buf[len - 1] == 0xD9 && buf[len - 2] == 0xFF)
      break;
    len--;
  }
  if (len <= 2)
    return PARSE_EXIF_ERROR_NO_JPEG;

  clear();

  // Scan for EXIF header (bytes 0xFF 0xE1) and do a sanity check by
  // looking for bytes "Exif\0\0". The marker length data is in Motorola
  // byte order, which results in the 'false' parameter to parse16().
  // The marker has to contain at least the TIFF header, otherwise the
  // EXIF data is corrupt. So the minimum length specified here has to be:
  //   2 bytes: section size
  //   6 bytes: "Exif\0\0" string
  //   2 bytes: TIFF header (either "II" or "MM" string)
  //   2 bytes: TIFF magic (short 0x2a00 in Motorola byte order)
  //   4 bytes: Offset to first IFD
  // =========
  //  16 bytes
  unsigned offs = 0;  // current offset into buffer
  for (offs = 0; offs < len - 1; offs++)
    if (buf[offs] == 0xFF && buf[offs + 1] == 0xE1) break;
  if (offs + 4 > len) return PARSE_EXIF_ERROR_NO_EXIF;
  offs += 2;
  unsigned short section_length = parse_value<uint16_t>(buf + offs, false);
  if (offs + section_length > len || section_length < 16)
    return PARSE_EXIF_ERROR_CORRUPT;
  offs += 2;

  return parseFromEXIFSegment(buf + offs, len - offs);
}

int easyexif::EXIFInfo::parseFrom(const string &data) {
  return parseFrom(
      reinterpret_cast<const unsigned char *>(data.data()), static_cast<unsigned>(data.length()));
}

//
// Main parsing function for an EXIF segment.
//
// PARAM: 'buf' start of the EXIF TIFF, which must be the bytes "Exif\0\0".
// PARAM: 'len' length of buffer
//
int easyexif::EXIFInfo::parseFromEXIFSegment(const unsigned char *buf,
                                             unsigned len) {
  bool alignIntel = true;  // byte alignment (defined in EXIF header)
  unsigned offs = 0;       // current offset into buffer
  if (!buf || len < 6) return PARSE_EXIF_ERROR_NO_EXIF;

  if (!std::equal(buf, buf + 6, "Exif\0\0")) return PARSE_EXIF_ERROR_NO_EXIF;
  offs += 6;

  // Now parsing the TIFF header. The first two bytes are either "II" or
  // "MM" for Intel or Motorola byte alignment. Sanity check by parsing
  // the unsigned short that follows, making sure it equals 0x2a. The
  // last 4 bytes are an offset into the first IFD, which are added to
  // the global offset counter. For this block, we expect the following
  // minimum size:
  //  2 bytes: 'II' or 'MM'
  //  2 bytes: 0x002a
  //  4 bytes: offset to first IDF
  // -----------------------------
  //  8 bytes
  if (offs + 8 > len) return PARSE_EXIF_ERROR_CORRUPT;
  unsigned tiff_header_start = offs;
  if (buf[offs] == 'I' && buf[offs + 1] == 'I')
    alignIntel = true;
  else {
    if (buf[offs] == 'M' && buf[offs + 1] == 'M')
      alignIntel = false;
    else
      return PARSE_EXIF_ERROR_UNKNOWN_BYTEALIGN;
  }
  this->ByteAlign = alignIntel;
  offs += 2;
  if (0x2a != parse_value<uint16_t>(buf + offs, alignIntel))
    return PARSE_EXIF_ERROR_CORRUPT;
  offs += 2;
  unsigned first_ifd_offset = parse_value<uint32_t>(buf + offs, alignIntel);
  offs += first_ifd_offset - 4;
  if (offs >= len) return PARSE_EXIF_ERROR_CORRUPT;

  // Now parsing the first Image File Directory (IFD0, for the main image).
  // An IFD consists of a variable number of 12-byte directory entries. The
  // first two bytes of the IFD section contain the number of directory
  // entries in the section. The last 4 bytes of the IFD contain an offset
  // to the next IFD, which means this IFD must contain exactly 6 + 12 * num
  // bytes of data.
  if (offs + 2 > len) return PARSE_EXIF_ERROR_CORRUPT;
  int num_entries = parse_value<uint16_t>(buf + offs, alignIntel);
  if (offs + 6 + 12 * num_entries > len) return PARSE_EXIF_ERROR_CORRUPT;
  offs += 2;
  unsigned exif_sub_ifd_offset = len;
  unsigned gps_sub_ifd_offset = len;
  while (--num_entries >= 0) {
    IFEntry result =
        parseIFEntry(buf, offs, alignIntel, tiff_header_start, len);
    offs += 12;
    switch (result.tag()) {
      case 0x102:
        // Bits per sample
        if (result.format() == 3 && result.val_short().size())
          this->BitsPerSample = result.val_short().front();
        break;

      case 0x10E:
        // Image description
        if (result.format() == 2) this->ImageDescription = result.val_string();
        break;

      case 0x10F:
        // Digicam make
        if (result.format() == 2) this->Make = result.val_string();
        break;

      case 0x110:
        // Digicam model
        if (result.format() == 2) this->Model = result.val_string();
        break;

      case 0x112:
        // Orientation of image
        if (result.format() == 3 && result.val_short().size())
          this->Orientation = result.val_short().front();
        break;

      case 0x131:
        // Software used for image
        if (result.format() == 2) this->Software = result.val_string();
        break;

      case 0x132:
        // EXIF/TIFF date/time of image modification
        if (result.format() == 2) this->DateTime = result.val_string();
        break;

      case 0x8298:
        // Copyright information
        if (result.format() == 2) this->Copyright = result.val_string();
        break;

      case 0x8825:
        // GPS IFS offset
        gps_sub_ifd_offset = tiff_header_start + result.data();
        break;

      case 0x8769:
        // EXIF SubIFD offset
        exif_sub_ifd_offset = tiff_header_start + result.data();
        break;
    }
  }

  // Jump to the EXIF SubIFD if it exists and parse all the information
  // there. Note that it's possible that the EXIF SubIFD doesn't exist.
  // The EXIF SubIFD contains most of the interesting information that a
  // typical user might want.
  if (exif_sub_ifd_offset + 4 <= len) {
    offs = exif_sub_ifd_offset;
    int num_sub_entries = parse_value<uint16_t>(buf + offs, alignIntel);
    if (offs + 6 + 12 * num_sub_entries > len) return PARSE_EXIF_ERROR_CORRUPT;
    offs += 2;
    while (--num_sub_entries >= 0) {
      IFEntry result =
          parseIFEntry(buf, offs, alignIntel, tiff_header_start, len);
      switch (result.tag()) {
        case 0x829a:
          // Exposure time in seconds
          if (result.format() == 5 && result.val_rational().size())
            this->ExposureTime = result.val_rational().front();
          break;

        case 0x829d:
          // FNumber
          if (result.format() == 5 && result.val_rational().size())
            this->FNumber = result.val_rational().front();
          break;

      case 0x8822:
        // Exposure Program
        if (result.format() == 3 && result.val_short().size())
          this->ExposureProgram = result.val_short().front();
        break;

        case 0x8827:
          // ISO Speed Rating
          if (result.format() == 3 && result.val_short().size())
            this->ISOSpeedRatings = result.val_short().front();
          break;

        case 0x9003:
          // Original date and time
          if (result.format() == 2)
            this->DateTimeOriginal = result.val_string();
          break;

        case 0x9004:
          // Digitization date and time
          if (result.format() == 2)
            this->DateTimeDigitized = result.val_string();
          break;

        case 0x9201:
          // Shutter speed value
          if (result.format() == 5 && result.val_rational().size())
            this->ShutterSpeedValue = result.val_rational().front();
          break;

        case 0x9204:
          // Exposure bias value
          if (result.format() == 5 && result.val_rational().size())
            this->ExposureBiasValue = result.val_rational().front();
          break;

        case 0x9206:
          // Subject distance
          if (result.format() == 5 && result.val_rational().size())
            this->SubjectDistance = result.val_rational().front();
          break;

        case 0x9209:
          // Flash used
          if (result.format() == 3 && result.val_short().size()) {
            uint16_t data = result.val_short().front();
            
            this->Flash = data & 1;
            this->FlashReturnedLight = (data & 6) >> 1;
            this->FlashMode = (data & 24) >> 3;
          }
          break;

        case 0x920a:
          // Focal length
          if (result.format() == 5 && result.val_rational().size())
            this->FocalLength = result.val_rational().front();
          break;

        case 0x9207:
          // Metering mode
          if (result.format() == 3 && result.val_short().size())
            this->MeteringMode = result.val_short().front();
          break;

		case 0x9286:
			// qlh UserComment
			if(result.format() == 7)
				this->UserComment = result.val_string();
			break;
        case 0x9291:
          // Subsecond original time
          if (result.format() == 2)
            this->SubSecTimeOriginal = result.val_string();
          break;

        case 0xa002:
          // EXIF Image width
          if (result.format() == 4 && result.val_long().size())
            this->ImageWidth = result.val_long().front();
          if (result.format() == 3 && result.val_short().size())
            this->ImageWidth = result.val_short().front();
          break;

        case 0xa003:
          // EXIF Image height
          if (result.format() == 4 && result.val_long().size())
            this->ImageHeight = result.val_long().front();
          if (result.format() == 3 && result.val_short().size())
            this->ImageHeight = result.val_short().front();
          break;

        case 0xa20e:
          // EXIF Focal plane X-resolution
          if (result.format() == 5) {
            this->LensInfo.FocalPlaneXResolution = result.val_rational()[0];
          }
          break;

        case 0xa20f:
          // EXIF Focal plane Y-resolution
          if (result.format() == 5) {
            this->LensInfo.FocalPlaneYResolution = result.val_rational()[0];
          }
          break;

        case 0xa210:
            // EXIF Focal plane resolution unit
            if (result.format() == 3 && result.val_short().size()) {
                this->LensInfo.FocalPlaneResolutionUnit = result.val_short().front();
            }
            break;

        case 0xa405:
          // Focal length in 35mm film
          if (result.format() == 3 && result.val_short().size())
            this->FocalLengthIn35mm = result.val_short().front();
          break;

        case 0xa432:
          // Focal length and FStop.
          if (result.format() == 5) {
            int sz = static_cast<unsigned>(result.val_rational().size());
            if (sz)
              this->LensInfo.FocalLengthMin = result.val_rational()[0];
            if (sz > 1)
              this->LensInfo.FocalLengthMax = result.val_rational()[1];
            if (sz > 2)
              this->LensInfo.FStopMin = result.val_rational()[2];
            if (sz > 3)
              this->LensInfo.FStopMax = result.val_rational()[3];
          }
          break;

        case 0xa433:
          // Lens make.
          if (result.format() == 2) {
            this->LensInfo.Make = result.val_string();
          }
          break;

        case 0xa434:
          // Lens model.
          if (result.format() == 2) {
            this->LensInfo.Model = result.val_string();
          }
          break;
      }
      offs += 12;
    }
  }

  // Jump to the GPS SubIFD if it exists and parse all the information
  // there. Note that it's possible that the GPS SubIFD doesn't exist.
  if (gps_sub_ifd_offset + 4 <= len) {
    offs = gps_sub_ifd_offset;
    int num_sub_entries = parse_value<uint16_t>(buf + offs, alignIntel);
    if (offs + 6 + 12 * num_sub_entries > len) return PARSE_EXIF_ERROR_CORRUPT;
    offs += 2;
    while (--num_sub_entries >= 0) {
      unsigned short tag, format;
      unsigned length, data;
      parseIFEntryHeader(buf + offs, alignIntel, tag, format, length, data);
      switch (tag) {
        case 1:
          // GPS north or south
          this->GeoLocation.LatComponents.direction = *(buf + offs + 8);
          if (this->GeoLocation.LatComponents.direction == 0) {
            this->GeoLocation.LatComponents.direction = '?';
          }
          if ('S' == this->GeoLocation.LatComponents.direction) {
            this->GeoLocation.Latitude = -this->GeoLocation.Latitude;
          }
          break;

        case 2:
          // GPS latitude
          if ((format == 5 || format == 10) && length == 3) {
            this->GeoLocation.LatComponents.degrees = parse_value<Rational>(
                buf + data + tiff_header_start, alignIntel);
            this->GeoLocation.LatComponents.minutes = parse_value<Rational>(
                buf + data + tiff_header_start + 8, alignIntel);
            this->GeoLocation.LatComponents.seconds = parse_value<Rational>(
                buf + data + tiff_header_start + 16, alignIntel);
            this->GeoLocation.Latitude =
                this->GeoLocation.LatComponents.degrees +
                this->GeoLocation.LatComponents.minutes / 60 +
                this->GeoLocation.LatComponents.seconds / 3600;
            if ('S' == this->GeoLocation.LatComponents.direction) {
              this->GeoLocation.Latitude = -this->GeoLocation.Latitude;
            }
          }
          break;

        case 3:
          // GPS east or west
          this->GeoLocation.LonComponents.direction = *(buf + offs + 8);
          if (this->GeoLocation.LonComponents.direction == 0) {
            this->GeoLocation.LonComponents.direction = '?';
          }
          if ('W' == this->GeoLocation.LonComponents.direction) {
            this->GeoLocation.Longitude = -this->GeoLocation.Longitude;
          }
          break;

        case 4:
          // GPS longitude
          if ((format == 5 || format == 10) && length == 3) {
            this->GeoLocation.LonComponents.degrees = parse_value<Rational>(
                buf + data + tiff_header_start, alignIntel);
            this->GeoLocation.LonComponents.minutes = parse_value<Rational>(
                buf + data + tiff_header_start + 8, alignIntel);
            this->GeoLocation.LonComponents.seconds = parse_value<Rational>(
                buf + data + tiff_header_start + 16, alignIntel);
            this->GeoLocation.Longitude =
                this->GeoLocation.LonComponents.degrees +
                this->GeoLocation.LonComponents.minutes / 60 +
                this->GeoLocation.LonComponents.seconds / 3600;
            if ('W' == this->GeoLocation.LonComponents.direction)
              this->GeoLocation.Longitude = -this->GeoLocation.Longitude;
          }
          break;

        case 5:
          // GPS altitude reference (below or above sea level)
          this->GeoLocation.AltitudeRef = *(buf + offs + 8);
          if (1 == this->GeoLocation.AltitudeRef) {
            this->GeoLocation.Altitude = -this->GeoLocation.Altitude;
          }
          break;

        case 6:
          // GPS altitude
          if ((format == 5 || format == 10)) {
            this->GeoLocation.Altitude = parse_value<Rational>(
                buf + data + tiff_header_start, alignIntel);
            if (1 == this->GeoLocation.AltitudeRef) {
              this->GeoLocation.Altitude = -this->GeoLocation.Altitude;
            }
          }
          break;

        case 11:
          // GPS degree of precision (DOP)
          if ((format == 5 || format == 10)) {
            this->GeoLocation.DOP = parse_value<Rational>(
                buf + data + tiff_header_start, alignIntel);
          }
          break;
      }
      offs += 12;
    }
  }

  return PARSE_EXIF_SUCCESS;
}


int easyexif::EXIFInfo::makeExifFile( char *ExifOut, UINT *totalLen ,ExifFileInfo *exifFileInfo){
	UCHAR *ExifInitialCount;
	UCHAR *tempExif = ( UCHAR * ) ExifOut;
	INT32 ExifSize;
	UINT santemp;
	UCHAR * startoftiff;
	UCHAR * IFD1OffsetAddress;
	UCHAR APP1Marker[2]=	 {0xff,0xe1};
	UCHAR ExifLen[4]={0};
	UCHAR Nentries[2]={8,0};
	UCHAR SubIFDNentries[2]={18,0};
	UCHAR IFD1Nentries[2]={6,0};
	UCHAR EndOfEntry[4]={0};

	//VARIABLES FOR THE MAKE OF THE CAMERA
	UCHAR	maketag[2]={0xf,0x1};
	UCHAR	makeformat[2]={0x2,0x0};
	UCHAR	Ncomponent[4]={32,0x0,0x0,0x0};
	char  make[32];
	UCHAR makeoffchar[4];
	UCHAR * offset;

	//VARIABLES FOR THE MODEL OF THE CAMERA
	UCHAR	modeltag[2]={0x10,0x1};
	UCHAR	modelformat[2]={0x2,0x0};
	UCHAR	NcomponentModel[4]={32,0x0,0x0,0x0};
	char  model[32];
	UCHAR modeloffchar[4];

	//VARIABLES FOR THE ORIENTATION OF THE CAMERA
	UCHAR	orientationtag[2]={0x12,0x1};
	UCHAR	orientationformat[2]={0x3,0x0};
	UCHAR	NcomponentOrientation[4]={0x1,0x0,0x0,0x0};
	UINT  Orientation[1];
	UCHAR	Orient[4] = {0};


	//VARIABLES FOR THE JPEG PROCESS
	UCHAR	Processtag[2]={0x00,0x02};
	UCHAR	Processformat[2]={0x3,0x0};
	UCHAR	NcomponentProcess[4]={0x1,0x0,0x0,0x0};
	UINT  Process[1];
	UCHAR	Proc[4] = {0};

	//VARIABLES FOR THE X-RESOLUTION OF THE IMAGE
	UCHAR	XResolutiontag[2]={0x1A,0x1};
	UCHAR	XResolutionformat[2]={0x5,0x0};
	UCHAR	NcomponentXResolution[4]={0x1,0x0,0x0,0x0};
	UINT  XResolutionNum[1];//={0x00000048};
	UINT  XResolutionDen[1];//={0x00000001};

	UCHAR XResolutionoffchar[4];
	UCHAR XResolutionNumChar[4];
	UCHAR XResolutionDenChar[4];

	//VARIABLES FOR THE Y-RESOLUTION OF THE IMAGE
	UCHAR	YResolutiontag[2]={0x1B,0x1};
	UCHAR	YResolutionformat[2]={0x5,0x0};
	UCHAR	NcomponentYResolution[4]={0x1,0x0,0x0,0x0};
	UINT  YResolutionNum[1];//={0x00000048};
	UINT  YResolutionDen[1];//={0x00000001};

	UCHAR YResolutionoffchar[4];
	UCHAR YResolutionNumChar[4];
	UCHAR YResolutionDenChar[4];

	//VARIABLES FOR THE RESOLUTION UNIT OF THE CAMERA
	UCHAR	RUnittag[2]={0x28,0x1};
	UCHAR	RUnitformat[2]={0x3,0x0};
	UCHAR	NcomponentRUnit[4]={0x1,0x0,0x0,0x0};
	UINT  RUnit[1];
	UCHAR	RUnitChar[4] = {0};


	//VARIABLES FOR THE VERSION NO OF THE SOFTWARE
	UCHAR	Versiontag[2]={0x31,0x1};
	UCHAR	Versionformat[2]={0x2,0x0};
	UCHAR	NcomponentVersion[4]={32,0x0,0x0,0x0};
	char  Version[32];//="version 1.2";
	UCHAR Versionoffchar[4];

	//VARIABLES FOR THE DATE/TIME
	UCHAR	DateTimetag[2]={0x32,0x1};
	UCHAR	DateTimeformat[2]={0x2,0x0};
	UCHAR	NcomponentDateTime[4]={20,0,0,0};
	UCHAR	DateTime[32];//="2006:6:09 15:17:32";
	char  DateTimeClose[1]={0};
	UCHAR DateTimeoffchar[4];

	//VARIABLES FOR THE COPYRIGHT
	UCHAR	CopyRighttag[2]={0x98,0x82};
	UCHAR	CopyRightformat[2]={0x2,0x0};
	UCHAR	NcomponentCopyRight[4]={32,0x0,0x0,0x0};
	char  CopyRight[32];
	UCHAR CopyRightoffchar[4];

	//VARIABLES FOR THE OFFSET TO SUBIFD
	UCHAR	SubIFDOffsettag[2]={0x69,0x87};
	UCHAR	SubIFDOffsetformat[2]={0x4,0x0};
	UCHAR	NcomponentSubIFDOffset[4]={0x1,0x0,0x0,0x0};
	UCHAR	SubIFDOffsetChar[4] = {0};


	//VARIABLES FOR THE EXPOSURE TIME
	UCHAR	ExposureTimetag[2]={0x9A,0x82};
	UCHAR	ExposureTimeformat[2]={0x5,0x0};
	UCHAR	NcomponentExposureTime[4]={0x1,0x0,0x0,0x0};
	UINT  ExposureTimeNum[1];
	UINT  ExposureTimeDen[1];

	UCHAR ExposureTimeoffchar[4];
	UCHAR ExposureTimeNumChar[4];
	UCHAR ExposureTimeDenChar[4];

	//VARIABLES FOR THE FNUMBER
	UCHAR	FNumbertag[2]={0x9D,0x82};
	UCHAR	FNumberformat[2]={0x5,0x0};
	UCHAR	NcomponentFNumber[4]={0x1,0x0,0x0,0x0};
	UINT  FNumberNum[1];
	UINT  FNumberDen[1];

	UCHAR FNumberoffchar[4];
	UCHAR FNumberNumChar[4];
	UCHAR FNumberDenChar[4];

	//VARIABLES FOR THE EXPOSURE PROGRAM OF THE CAMERA
	UCHAR	ExposureProgramtag[2]={0x22,0x88};
	UCHAR	ExposureProgramformat[2]={0x3,0x0};
	UCHAR	NcomponentExposureProgram[4]={0x1,0x0,0x0,0x0};
	UINT  ExposureProgram[1];
	UCHAR	ExposureProgramChar[4] = {0};

	//VARIABLES FOR THE ISO SPEED RATINGS OF THE CAMERA
	UCHAR	ISOSpeedRatingstag[2]={0x27,0x88};
	UCHAR	ISOSpeedRatingsformat[2]={0x3,0x0};
	UCHAR	NcomponentISOSpeedRatings[4]={0x2,0x0,0x0,0x0};
	unsigned short   ISOSpeedRatings[2];
	UCHAR	ISOSpeedRatingsChar[4] = {0};

	//VARIABLES FOR THE BRIGHTNESS OF THE IMAGE
	UCHAR	Brightnesstag[2]={0x03,0x92};
	UCHAR	Brightnessformat[2]={0xA,0x0};
	UCHAR	NcomponentBrightness[4]={0x1,0x0,0x0,0x0};
	int BrightnessNum[1];
	int BrightnessDen[1];

	UCHAR Brightnessoffchar[4];
	UCHAR BrightnessNumChar[4];
	UCHAR BrightnessDenChar[4];

	//VARIABLES FOR THE EXPOSURE Bias
	UCHAR	ExposureBiastag[2]={0x04,0x92};
	UCHAR	ExposureBiasformat[2]={0xA,0x0};
	UCHAR	NcomponentExposureBias[4]={0x1,0x0,0x0,0x0};
	int ExposureBiasNum[1];//={-8};
	int ExposureBiasDen[1];//={1};

	UCHAR ExposureBiasoffchar[4];
	UCHAR ExposureBiasNumChar[4];
	UCHAR ExposureBiasDenChar[4];

	//VARIABLES FOR THE SUBJECT DISTANCE OF THE IMAGE
	UCHAR	SubjectDistancetag[2]={0x06,0x92};
	UCHAR	SubjectDistanceformat[2]={0xA,0x0};
	UCHAR	NcomponentSubjectDistance[4]={0x1,0x0,0x0,0x0};
	int SubjectDistanceNum[1];
	int SubjectDistanceDen[1];

	UCHAR SubjectDistanceoffchar[4];
	UCHAR SubjectDistanceNumChar[4];
	UCHAR SubjectDistanceDenChar[4];

	//VARIABLES FOR THE METERING MODE
	UCHAR	MeteringModetag[2]={0x07,0x92};
	UCHAR	MeteringModeformat[2]={0x3,0x0};
	UCHAR	NcomponentMeteringMode[4]={0x1,0x0,0x0,0x0};
	UINT	MeteringMode[1];
	UCHAR	MeteringModeChar[4] = {0};

	//VARIABLES FOR THE FLASH
	UCHAR	Flashtag[2]={0x09,0x92};
	UCHAR	Flashformat[2]={0x3,0x0};
	UCHAR	NcomponentFlash[4]={0x1,0x0,0x0,0x0};
	UINT	Flash[1]={1};
	UCHAR	FlashChar[4] = {0};

	//VARIABLES FOR THE FOCAL LENGTH
	UCHAR	FocalLengthtag[2]={0x0A,0x92};
	UCHAR	FocalLengthformat[2]={0x5,0x0};
	UCHAR	NcomponentFocalLength[4]={0x1,0x0,0x0,0x0};
	UINT FocalLengthNum[1];
	UINT FocalLengthDen[1];

	UCHAR FocalLengthoffchar[4];
	UCHAR FocalLengthNumChar[4];
	UCHAR FocalLengthDenChar[4];

	//VARIABLES FOR THE ISO WIDTH OF THE MAIN IMAGE
	UCHAR	Widthtag[2]={0x02,0xA0};
	UCHAR	Widthformat[2]={0x3,0x0};
	UCHAR	NcomponentWidth[4]={0x1,0x0,0x0,0x0};
	UINT	Width[1];
	UCHAR	WidthChar[4] = {0};

	//VARIABLES FOR THE ISO HEIGHT OF THE MAIN IMAGE
	UCHAR	Heighttag[2]={0x03,0xA0};
	UCHAR	Heightformat[2]={0x3,0x0};
	UCHAR	NcomponentHeight[4]={0x1,0x0,0x0,0x0};
	UINT	Height[1];
	UCHAR	HeightChar[4] = {0};

	//VARIABLES FOR THE COLORSPACE
	UCHAR	ColorSpacetag[2]={0x01,0xA0};
	//char  ColorSpacetag[2]={0x54,0x56};
	UCHAR	ColorSpaceformat[2]={0x3,0x0};
	UCHAR	NcomponentColorSpace[4]={0x1,0x0,0x0,0x0};
	UINT	ColorSpace[1];//={1};
	UCHAR	ColorSpaceChar[4] = {0};

	//VARIABLES FOR THE FocalPlaneXResolution
	UCHAR	FocalPlaneXResolutiontag[2]={0x0E,0xA2};
	UCHAR	FocalPlaneXResolutionformat[2]={0x5,0x0};
	UCHAR	NcomponentFocalPlaneXResolution[4]={0x1,0x0,0x0,0x0};
	UINT FocalPlaneXResolutionNum[1];
	UINT FocalPlaneXResolutionDen[1];

	UCHAR FocalPlaneXResolutionoffchar[4];
	UCHAR FocalPlaneXResolutionNumChar[4];
	UCHAR FocalPlaneXResolutionDenChar[4];

	//VARIABLES FOR THE FocalPlaneYResolution
	UCHAR	FocalPlaneYResolutiontag[2]={0x0F,0xA2};
	UCHAR	FocalPlaneYResolutionformat[2]={0x5,0x0};
	UCHAR	NcomponentFocalPlaneYResolution[4]={0x1,0x0,0x0,0x0};
	UINT FocalPlaneYResolutionNum[1];
	UINT FocalPlaneYResolutionDen[1];

	UCHAR FocalPlaneYResolutionoffchar[4];
	UCHAR FocalPlaneYResolutionNumChar[4];
	UCHAR FocalPlaneYResolutionDenChar[4];

	//VARIABLES FOR THE FocalPlaneResolutionUnit
	UCHAR	FocalPlaneResolutionUnittag[2]={0x10,0xA2};
	UCHAR	FocalPlaneResolutionUnitformat[2]={0x3,0x0};
	UCHAR	NcomponentFocalPlaneResolutionUnit[4]={0x1,0x0,0x0,0x0};
	UINT	FocalPlaneResolutionUnit[1];
	UCHAR	FocalPlaneResolutionUnitChar[4] = {0};


	//VARIABLES FOR THE WHITE BALANCE PROGRAM OF THE CAMERA
	UCHAR	WhiteBalancetag[2]={0x07,0x00};
	UCHAR	WhiteBalanceformat[2]={0x3,0x0};
	UCHAR	NcomponentWhiteBalance[4]={0x1,0x0,0x0,0x0};
	UINT WhiteBalance[1];
	UCHAR	WhiteBalanceChar[4] = {0};

	//VARIABLES FOR THE USER COMMENTS
	UCHAR	UserCommentstag[2]={0x86,0x92};
	UCHAR	UserCommentsformat[2]={0x7,0x0};
	UCHAR	NcomponentUserComments[4]={150,0x0,0x0,0x0};
	UCHAR	UserComments[150];
	UCHAR UserCommentsoffchar[4];

	//VARIABLES FOR THE COMPRESSION TYPE
	UCHAR	Compressiontag[2]={0x03,0x01};
	UCHAR	Compressionformat[2]={0x3,0x0};
	UCHAR	NcomponentCompression[4]={0x1,0x0,0x0,0x0};
	UINT	Compression[1]={6};
	UCHAR	CompressionChar[4] = {0};

	//VARIABLES FOR THE JpegIFOffset
	UCHAR	JpegIFOffsettag[2]={0x01,0x02};
	UCHAR	JpegIFOffsetformat[2]={0x4,0x0};
	UCHAR	NcomponentJpegIFOffset[4]={0x1,0x0,0x0,0x0};
	UCHAR	JpegIFOffsetChar[4] = {0};

	//VARIABLES FOR THE JpegIFByteCount
	UCHAR	JpegIFByteCounttag[2]={0x02,0x02};
	UCHAR	JpegIFByteCountformat[2]={0x4,0x0};
	UCHAR	NcomponentJpegIFByteCount[4]={0x1,0x0,0x0,0x0};
	UCHAR	JpegIFByteCountChar[4] = {0};
	//END OF THE VARIABLES

	ExifInitialCount=tempExif;
	//for APP1 Marker(2 byte) and length(2 byte)
	tempExif += 4;
	//write an exif header
	memcpy ( tempExif, ExifHeader, 6 );
	tempExif += 6 ;

	//write a tiff header
	memcpy ( tempExif, TIFFHeader, 8 );
	startoftiff=tempExif;
	tempExif += 8 ;
	//write no of entries in 1d0
	memcpy ( tempExif, Nentries, 2 );
	tempExif += 2 ;
	///////////////ENTRY NO 1 :MAKE OF CAMERA////////////////////////
	//write make tag
	memcpy ( tempExif, maketag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, makeformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, Ncomponent, 4 );
	tempExif += 4 ;
	//write make
	//strcpy(make,tpJEInfo->Make);
	memcpy ( make, exifFileInfo->Make,32 );
	offset = ( UCHAR * ) 0x200;
	santemp= ( int ) ( offset );
	makeoffchar[0]= ( unsigned char ) santemp;
	makeoffchar[1]= ( unsigned char ) ( santemp>>8 );
	makeoffchar[2]= ( unsigned char ) ( santemp>>16 );
	makeoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the make offset into the bitstream
	memcpy ( tempExif, makeoffchar, 4 );
	tempExif += 4 ;
	memcpy ( startoftiff+santemp, make, 32 );
	offset+=32;

	///////////////ENTRY NO 2 :MODEL OF CAMERA////////////////////////
	//write model tag
	memcpy ( tempExif, modeltag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, modelformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentModel, 4 ); //sanjeev
	tempExif += 4 ;
	//write model
	//  strcpy(model,tpJEInfo->Model);
	memcpy ( model,exifFileInfo->Model,32 );
	santemp= ( int ) ( offset );
	modeloffchar[0]= ( unsigned char ) santemp;
	modeloffchar[1]= ( unsigned char ) ( santemp>>8 );
	modeloffchar[2]= ( unsigned char ) ( santemp>>16 );
	modeloffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the model offset into the bitstream
	memcpy ( tempExif, modeloffchar, 4 );
	tempExif += 4 ;
	memcpy ( startoftiff+santemp, model, 32 );
	offset+=32;


	///////////////ENTRY NO 3 :ORIENTATION OF CAMERA////////////////////////
	//write orientation tag
	memcpy ( tempExif, orientationtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, orientationformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentOrientation, 4 );
	tempExif += 4 ;
	//write orientation mode
	Orientation[0] =exifFileInfo->Orientation;
	Orient[0] = ( unsigned char ) ( Orientation[0] );
	Orient[1] = ( unsigned char ) ( Orientation[0]>>8 );
	Orient[2] = ( unsigned char ) ( Orientation[0]>>16 );
	Orient[3] = ( unsigned char ) ( Orientation[0]>>24 );

	memcpy ( tempExif, Orient, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 4 :JPEG PROCESS////////////////////////
	//write orientation tag
	memcpy ( tempExif, Processtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Processformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentProcess, 4 );
	tempExif += 4 ;
	//write orientation mode
	Process[0] =exifFileInfo->Process;
	Proc[0] = ( unsigned char ) ( Process[0] );
	Proc[1] = ( unsigned char ) ( Process[0]>>8 );
	Proc[2] = ( unsigned char ) ( Process[0]>>16 );
	Proc[3] = ( unsigned char ) ( Process[0]>>24 );

	memcpy ( tempExif, Proc, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 5 :VERSION OF software////////////////////////
	//write VERSION tag
	memcpy ( tempExif, Versiontag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Versionformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentVersion, 4 ); //sanjeev
	tempExif += 4 ;


	santemp= ( int ) ( offset );
	Versionoffchar[0]= ( unsigned char ) santemp;
	Versionoffchar[1]= ( unsigned char ) ( santemp>>8 );
	Versionoffchar[2]= ( unsigned char ) ( santemp>>16 );
	Versionoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the VERSION offset into the bitstream
	memcpy ( tempExif, Versionoffchar, 4 );
	tempExif += 4 ;
	//  strcpy(Version,jCtx->ExifInfo->Version);
	memcpy ( Version,exifFileInfo->Version,32 );
	memcpy ( startoftiff+santemp, Version, 32 );
	offset+=32;
	///////////////ENTRY NO 6 :Date/Time////////////////////////
	//write model tag
	memcpy ( tempExif, DateTimetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, DateTimeformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentDateTime, 4 ); //sanjeev
	tempExif += 4 ;
	//write Date/Time
	//strcpy(DateTime,jCtx->ExifInfo->DateTime);
	memcpy ( DateTime,exifFileInfo->DateTime,20 );

	santemp= ( int ) ( offset );
	DateTimeoffchar[0]= ( unsigned char ) santemp;
	DateTimeoffchar[1]= ( unsigned char ) ( santemp>>8 );
	DateTimeoffchar[2]= ( unsigned char ) ( santemp>>16 );
	DateTimeoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the model offset into the bitstream
	memcpy ( tempExif, DateTimeoffchar, 4 );
	tempExif += 4 ;
	memcpy ( startoftiff+santemp, DateTime, 19 );
	memcpy ( startoftiff+santemp+19, DateTimeClose, 1 );

	offset+=32;
	///////////////ENTRY NO 7 :COPYRIGHT INFO////////////////////////
	//write model tag
	memcpy ( tempExif, CopyRighttag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, CopyRightformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentCopyRight, 4 ); //sanjeev
	tempExif += 4 ;

	//  strcpy(CopyRight,jCtx->ExifInfo->CopyRight);="copyright 2006";);
	memcpy ( CopyRight,exifFileInfo->CopyRight,32 );

	santemp= ( int ) ( offset );
	CopyRightoffchar[0]= ( unsigned char ) santemp;
	CopyRightoffchar[1]= ( unsigned char ) ( santemp>>8 );
	CopyRightoffchar[2]= ( unsigned char ) ( santemp>>16 );
	CopyRightoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the model offset into the bitstream
	memcpy ( tempExif, CopyRightoffchar, 4 );
	tempExif += 4 ;
	memcpy ( startoftiff+santemp, CopyRight, 32 );

	offset+=32;
	///////////////ENTRY NO 8 :OFFSET TO THE SubIFD ////////////////////////
	//write SubIFD tag
	memcpy ( tempExif, SubIFDOffsettag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, SubIFDOffsetformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentSubIFDOffset, 4 );
	tempExif += 4 ;
	//write the  offset to the SubIFD
	santemp= ( int ) ( tempExif-startoftiff+8 );
	SubIFDOffsetChar[0] = ( unsigned char ) ( santemp );
	SubIFDOffsetChar[1] = ( unsigned char ) ( santemp>>8 );
	SubIFDOffsetChar[2] = ( unsigned char ) ( santemp>>16 );
	SubIFDOffsetChar[3] = ( unsigned char ) ( santemp>>24 );

	memcpy ( tempExif, SubIFDOffsetChar, 4 );
	tempExif += 4 ;


	// since it was the last directory entry, so next 4 bytes contains an offset to the IFD1.

	//since we dont know the offset lets put 0x0000 as an offset, later when get to know the
	//actual offset we will revisit here and put the actual offset.
	santemp=0x0000;
	SubIFDOffsetChar[0] = ( unsigned char ) ( santemp );
	SubIFDOffsetChar[1] = ( unsigned char ) ( santemp>>8 );
	SubIFDOffsetChar[2] = ( unsigned char ) ( santemp>>16 );
	SubIFDOffsetChar[3] = ( unsigned char ) ( santemp>>24 );
	IFD1OffsetAddress = tempExif;
	memcpy ( tempExif, SubIFDOffsetChar, 4 );
	tempExif += 4 ;
	/////////////EXIF SUBIFD STARTS HERE//////////////////////////////////
	//write no of entries in SubIFD
	memcpy ( tempExif, SubIFDNentries, 2 );
	tempExif += 2 ;
	///////////////ENTRY NO 1 : EXPOSURE TIME////////////////////////
	//write EXPOSURE TIME tag
	memcpy ( tempExif, ExposureTimetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, ExposureTimeformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentExposureTime, 4 );
	tempExif += 4 ;
	//write EXPOSURE TIME

	santemp= ( int ) ( offset );
	ExposureTimeoffchar[0]= ( unsigned char ) santemp;
	ExposureTimeoffchar[1]= ( unsigned char ) ( santemp>>8 );
	ExposureTimeoffchar[2]= ( unsigned char ) ( santemp>>16 );
	ExposureTimeoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the X-Resolution offset into the bitstream
	memcpy ( tempExif, ExposureTimeoffchar, 4 );
	tempExif += 4 ;

	ExposureTimeNum[0]=exifFileInfo->ExposureTimeNum;
	ExposureTimeDen[0]=exifFileInfo->ExposureTimeDen;
	ExposureTimeNumChar[0]= ( unsigned char ) ExposureTimeNum[0];
	ExposureTimeNumChar[1]= ( unsigned char ) ( ExposureTimeNum[0]>>8 );
	ExposureTimeNumChar[2]= ( unsigned char ) ( ExposureTimeNum[0]>>16 );
	ExposureTimeNumChar[3]= ( unsigned char ) ( ExposureTimeNum[0]>>24 );

	ExposureTimeDenChar[0]= ( unsigned char ) ExposureTimeDen[0];
	ExposureTimeDenChar[1]= ( unsigned char ) ( ExposureTimeDen[0]>>8 );
	ExposureTimeDenChar[2]= ( unsigned char ) ( ExposureTimeDen[0]>>16 );
	ExposureTimeDenChar[3]= ( unsigned char ) ( ExposureTimeDen[0]>>24 );

	//WRITE THE EXPOSURE TIME NUMERATOR
	memcpy ( startoftiff+santemp, ExposureTimeNumChar, 4 );

	memcpy ( startoftiff+santemp+4, ExposureTimeDenChar, 4 );

	offset+=32;
	///////////////ENTRY NO 2 : F NUMBER////////////////////////
	//write FNumber tag
	memcpy ( tempExif, FNumbertag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, FNumberformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFNumber, 4 ); //sanjeev
	tempExif += 4 ;
	//write F NUMBER

	santemp= ( int ) ( offset );
	FNumberoffchar[0]= ( unsigned char ) santemp;
	FNumberoffchar[1]= ( unsigned char ) ( santemp>>8 );
	FNumberoffchar[2]= ( unsigned char ) ( santemp>>16 );
	FNumberoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the F NUMBER into the bitstream
	memcpy ( tempExif, FNumberoffchar, 4 );
	tempExif += 4 ;

	FNumberNum[0]=exifFileInfo->FNumberNum;
	FNumberDen[0]=exifFileInfo->FNumberDen;

	FNumberNumChar[0]= ( unsigned char ) FNumberNum[0];
	FNumberNumChar[1]= ( unsigned char ) ( FNumberNum[0]>>8 );
	FNumberNumChar[2]= ( unsigned char ) ( FNumberNum[0]>>16 );
	FNumberNumChar[3]= ( unsigned char ) ( FNumberNum[0]>>24 );

	FNumberDenChar[0]= ( unsigned char ) FNumberDen[0];
	FNumberDenChar[1]= ( unsigned char ) ( FNumberDen[0]>>8 );
	FNumberDenChar[2]= ( unsigned char ) ( FNumberDen[0]>>16 );
	FNumberDenChar[3]= ( unsigned char ) ( FNumberDen[0]>>24 );

	//WRITE THE FNumber NUMERATOR
	memcpy ( startoftiff+santemp, FNumberNumChar, 4 );

	memcpy ( startoftiff+santemp+4, FNumberDenChar, 4 );

	offset+=32;
	///////////////ENTRY NO 3 :EXPOSURE PROGRAM////////////////////////
	//write ExposureProgram tag
	memcpy ( tempExif, ExposureProgramtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, ExposureProgramformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentExposureProgram, 4 );
	tempExif += 4 ;
	//write orientation mode
	ExposureProgram[0] =exifFileInfo->ExposureProgram;

	ExposureProgramChar[0] = ( unsigned char ) ( ExposureProgram[0] );
	ExposureProgramChar[1] = ( unsigned char ) ( ExposureProgram[0]>>8 );
	ExposureProgramChar[2] = ( unsigned char ) ( ExposureProgram[0]>>16 );
	ExposureProgramChar[3] = ( unsigned char ) ( ExposureProgram[0]>>24 );

	memcpy ( tempExif, ExposureProgramChar, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 4 :ISOSpeedRatings ////////////////////////
	//write ISOSpeedRatings  tag
	memcpy ( tempExif, ISOSpeedRatingstag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, ISOSpeedRatingsformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentISOSpeedRatings, 4 );
	tempExif += 4 ;
	//write orientation mode
	ISOSpeedRatings[0] = 1;
	ISOSpeedRatings[1] = 2;

	ISOSpeedRatingsChar[0] = ( unsigned char ) ( ISOSpeedRatings[0] );
	ISOSpeedRatingsChar[1] = ( unsigned char ) ( ISOSpeedRatings[0]>>8 );
	ISOSpeedRatingsChar[2] = ( unsigned char ) ( ISOSpeedRatings[1] );
	ISOSpeedRatingsChar[3] = ( unsigned char ) ( ISOSpeedRatings[1]>>8 );

	memcpy ( tempExif, ISOSpeedRatingsChar, 4 );
	tempExif += 4 ;


	///////////////ENTRY NO 5 : BRIGHTNESS OF THE IMAGE////////////////////////
	//write BRIGHTNESS tag
	memcpy ( tempExif, Brightnesstag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Brightnessformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentBrightness, 4 ); //sanjeev
	tempExif += 4 ;
	//write X - Resolution

	santemp= ( int ) ( offset );
	Brightnessoffchar[0]= ( unsigned char ) santemp;
	Brightnessoffchar[1]= ( unsigned char ) ( santemp>>8 );
	Brightnessoffchar[2]= ( unsigned char ) ( santemp>>16 );
	Brightnessoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the X-Resolution offset into the bitstream
	memcpy ( tempExif, Brightnessoffchar, 4 );
	tempExif += 4 ;

	BrightnessNum[0] = exifFileInfo->BrightnessNum;
	BrightnessDen[0] = exifFileInfo->BrightnessDen;

	BrightnessNumChar[0]= ( unsigned char ) BrightnessNum[0];
	BrightnessNumChar[1]= ( unsigned char ) ( BrightnessNum[0]>>8 );
	BrightnessNumChar[2]= ( unsigned char ) ( BrightnessNum[0]>>16 );
	BrightnessNumChar[3]= ( unsigned char ) ( BrightnessNum[0]>>24 );

	BrightnessDenChar[0]= ( unsigned char ) BrightnessDen[0];
	BrightnessDenChar[1]= ( unsigned char ) ( BrightnessDen[0]>>8 );
	BrightnessDenChar[2]= ( unsigned char ) ( BrightnessDen[0]>>16 );
	BrightnessDenChar[3]= ( unsigned char ) ( BrightnessDen[0]>>24 );

	//WRITE THE X - RESOLUTION NUMERATOR
	memcpy ( startoftiff+santemp, BrightnessNumChar, 4 );

	memcpy ( startoftiff+santemp+4, BrightnessDenChar, 4 );

	offset+=48;

	///////////////ENTRY NO 6 : EXPOSURE BIAS OF THE IMAGE////////////////////////
	//write BRIGHTNESS tag
	memcpy ( tempExif, ExposureBiastag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, ExposureBiasformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentExposureBias, 4 ); //sanjeev
	tempExif += 4 ;
	//write EXPOSURE BIAS


	santemp= ( int ) ( offset );
	ExposureBiasoffchar[0]= ( unsigned char ) santemp;
	ExposureBiasoffchar[1]= ( unsigned char ) ( santemp>>8 );
	ExposureBiasoffchar[2]= ( unsigned char ) ( santemp>>16 );
	ExposureBiasoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the EXPOSURE BIAS offset into the bitstream
	memcpy ( tempExif, ExposureBiasoffchar, 4 );
	tempExif += 4 ;
	ExposureBiasNum[0]=exifFileInfo->ExposureBiasNum;
	ExposureBiasDen[0]=exifFileInfo->ExposureBiasDen;
	ExposureBiasNumChar[0]= ( unsigned char ) ExposureBiasNum[0];
	ExposureBiasNumChar[1]= ( unsigned char ) ( ExposureBiasNum[0]>>8 );
	ExposureBiasNumChar[2]= ( unsigned char ) ( ExposureBiasNum[0]>>16 );
	ExposureBiasNumChar[3]= ( unsigned char ) ( ExposureBiasNum[0]>>24 );

	ExposureBiasDenChar[0]= ( unsigned char ) ExposureBiasDen[0];
	ExposureBiasDenChar[1]= ( unsigned char ) ( ExposureBiasDen[0]>>8 );
	ExposureBiasDenChar[2]= ( unsigned char ) ( ExposureBiasDen[0]>>16 );
	ExposureBiasDenChar[3]= ( unsigned char ) ( ExposureBiasDen[0]>>24 );

	//WRITE THE EXPOSURE BIAS NUMERATOR
	memcpy ( startoftiff+santemp, ExposureBiasNumChar, 4 );

	memcpy ( startoftiff+santemp+4, ExposureBiasDenChar, 4 );

	offset+=48;
	///////////////ENTRY NO 7 : SUBJECT DISTANCE////////////////////////
	//write SUBJECT DISTANCE tag
	memcpy ( tempExif, SubjectDistancetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, SubjectDistanceformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentSubjectDistance, 4 ); //sanjeev
	tempExif += 4 ;
	//write SUBJECT DISTANCE


	santemp= ( int ) ( offset );
	SubjectDistanceoffchar[0]= ( unsigned char ) santemp;
	SubjectDistanceoffchar[1]= ( unsigned char ) ( santemp>>8 );
	SubjectDistanceoffchar[2]= ( unsigned char ) ( santemp>>16 );
	SubjectDistanceoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the SUBJECT DISTANCE offset into the bitstream
	memcpy ( tempExif, SubjectDistanceoffchar, 4 );
	tempExif += 4 ;
	SubjectDistanceNum[0]=exifFileInfo->SubjectDistanceNum;
	SubjectDistanceDen[0]=exifFileInfo->SubjectDistanceDen;
	SubjectDistanceNumChar[0]= ( unsigned char ) SubjectDistanceNum[0];
	SubjectDistanceNumChar[1]= ( unsigned char ) ( SubjectDistanceNum[0]>>8 );
	SubjectDistanceNumChar[2]= ( unsigned char ) ( SubjectDistanceNum[0]>>16 );
	SubjectDistanceNumChar[3]= ( unsigned char ) ( SubjectDistanceNum[0]>>24 );

	SubjectDistanceDenChar[0]= ( unsigned char ) SubjectDistanceDen[0];
	SubjectDistanceDenChar[1]= ( unsigned char ) ( SubjectDistanceDen[0]>>8 );
	SubjectDistanceDenChar[2]= ( unsigned char ) ( SubjectDistanceDen[0]>>16 );
	SubjectDistanceDenChar[3]= ( unsigned char ) ( SubjectDistanceDen[0]>>24 );

	//WRITE THE SUBJECT DISTANCE NUMERATOR
	memcpy ( startoftiff+santemp, SubjectDistanceNumChar, 4 );

	memcpy ( startoftiff+santemp+4, SubjectDistanceDenChar, 4 );

	offset+=48;

	///////////////ENTRY NO 8 :METERING MODE////////////////////////
	//write METERING tag
	memcpy ( tempExif, MeteringModetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, MeteringModeformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentMeteringMode, 4 );
	tempExif += 4 ;
	//write METERING mode
	MeteringMode[0] = exifFileInfo->MeteringMode;
	MeteringModeChar[0] = ( unsigned char ) ( MeteringMode[0] );
	MeteringModeChar[1] = ( unsigned char ) ( MeteringMode[0]>>8 );
	MeteringModeChar[2] = ( unsigned char ) ( MeteringMode[0]>>16 );
	MeteringModeChar[3] = ( unsigned char ) ( MeteringMode[0]>>24 );

	memcpy ( tempExif, MeteringModeChar, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 9 :FLASH////////////////////////
	//write FLASH tag
	memcpy ( tempExif, Flashtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Flashformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFlash, 4 );
	tempExif += 4 ;
	//write FLASH mode
	Flash[0]= exifFileInfo->Flash;
	FlashChar[0] = ( unsigned char ) ( Flash[0] );
	FlashChar[1] = ( unsigned char ) ( Flash[0]>>8 );
	FlashChar[2] = ( unsigned char ) ( Flash[0]>>16 );
	FlashChar[3] = ( unsigned char ) ( Flash[0]>>24 );

	memcpy ( tempExif, FlashChar, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 10 : FOCAL LENGTH////////////////////////
	//write FOCAL LENGTH tag
	memcpy ( tempExif, FocalLengthtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, FocalLengthformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFocalLength, 4 ); //sanjeev
	tempExif += 4 ;
	//write FOCAL LENGTH

	santemp= ( int ) ( offset );
	FocalLengthoffchar[0]= ( unsigned char ) santemp;
	FocalLengthoffchar[1]= ( unsigned char ) ( santemp>>8 );
	FocalLengthoffchar[2]= ( unsigned char ) ( santemp>>16 );
	FocalLengthoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the FOCAL LENGTH offset into the bitstream
	memcpy ( tempExif, FocalLengthoffchar, 4 );
	tempExif += 4 ;
	FocalLengthNum[0]=exifFileInfo->FocalLengthNum;
	FocalLengthDen[0]=exifFileInfo->FocalLengthDen;
	FocalLengthNumChar[0]= ( unsigned char ) FocalLengthNum[0];
	FocalLengthNumChar[1]= ( unsigned char ) ( FocalLengthNum[0]>>8 );
	FocalLengthNumChar[2]= ( unsigned char ) ( FocalLengthNum[0]>>16 );
	FocalLengthNumChar[3]= ( unsigned char ) ( FocalLengthNum[0]>>24 );

	FocalLengthDenChar[0]= ( unsigned char ) FocalLengthDen[0];
	FocalLengthDenChar[1]= ( unsigned char ) ( FocalLengthDen[0]>>8 );
	FocalLengthDenChar[2]= ( unsigned char ) ( FocalLengthDen[0]>>16 );
	FocalLengthDenChar[3]= ( unsigned char ) ( FocalLengthDen[0]>>24 );

	//WRITE THE FOCAL LENGTH NUMERATOR
	memcpy ( startoftiff+santemp, FocalLengthNumChar, 4 );

	memcpy ( startoftiff+santemp+4, FocalLengthDenChar, 4 );

	offset+=48;

	///////////////ENTRY NO 11 :Width////////////////////////
	//write Width tag
	memcpy ( tempExif, Widthtag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Widthformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentWidth, 4 );
	tempExif += 4 ;
	//write Width
	Width[0]=exifFileInfo->Width;
	WidthChar[0] = ( unsigned char ) ( Width[0] );
	WidthChar[1] = ( unsigned char ) ( Width[0]>>8 );
	WidthChar[2] = ( unsigned char ) ( Width[0]>>16 );
	WidthChar[3] = ( unsigned char ) ( Width[0]>>24 );

	memcpy ( tempExif, WidthChar, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 12 :Height////////////////////////
	//write Height tag
	memcpy ( tempExif, Heighttag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, Heightformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentHeight, 4 );
	tempExif += 4 ;
	//write Height
	Height[0]=exifFileInfo->Height;
	HeightChar[0] = ( unsigned char ) ( Height[0] );
	HeightChar[1] = ( unsigned char ) ( Height[0]>>8 );
	HeightChar[2] = ( unsigned char ) ( Height[0]>>16 );
	HeightChar[3] = ( unsigned char ) ( Height[0]>>24 );

	memcpy ( tempExif, HeightChar, 4 );
	tempExif += 4 ;

	///////////////ENTRY NO 13 :COLORSPACE////////////////////////
	//write ExposureProgram tag
	memcpy ( tempExif, ColorSpacetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, ColorSpaceformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentColorSpace, 4 );
	tempExif += 4 ;
	//write orientation mode
	ColorSpace [0]= exifFileInfo->ColorSpace;
	ColorSpaceChar[0] = ( unsigned char ) ( ColorSpace[0] );
	ColorSpaceChar[1] = ( unsigned char ) ( ColorSpace[0]>>8 );
	ColorSpaceChar[2] = ( unsigned char ) ( ColorSpace[0]>>16 );
	ColorSpaceChar[3] = ( unsigned char ) ( ColorSpace[0]>>24 );

	memcpy ( tempExif, ColorSpaceChar, 4 );
	tempExif += 4 ;
	///////////////ENTRY NO 14 : FocalPlaneXResolution////////////////////////
	//write EXPOSURE TIME tag
	memcpy ( tempExif, FocalPlaneXResolutiontag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, FocalPlaneXResolutionformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFocalPlaneXResolution, 4 );
	tempExif += 4 ;
	//write EXPOSURE TIME

	santemp= ( int ) ( offset );
	FocalPlaneXResolutionoffchar[0]= ( unsigned char ) santemp;
	FocalPlaneXResolutionoffchar[1]= ( unsigned char ) ( santemp>>8 );
	FocalPlaneXResolutionoffchar[2]= ( unsigned char ) ( santemp>>16 );
	FocalPlaneXResolutionoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the X-Resolution offset into the bitstream
	memcpy ( tempExif, FocalPlaneXResolutionoffchar, 4 );
	tempExif += 4 ;
	FocalPlaneXResolutionNum[0] = exifFileInfo->FocalPlaneXResolutionNum;
	FocalPlaneXResolutionDen[0] = exifFileInfo->FocalPlaneXResolutionDen;

	FocalPlaneXResolutionNumChar[0]= ( unsigned char ) FocalPlaneXResolutionNum[0];
	FocalPlaneXResolutionNumChar[1]= ( unsigned char ) ( FocalPlaneXResolutionNum[0]>>8 );
	FocalPlaneXResolutionNumChar[2]= ( unsigned char ) ( FocalPlaneXResolutionNum[0]>>16 );
	FocalPlaneXResolutionNumChar[3]= ( unsigned char ) ( FocalPlaneXResolutionNum[0]>>24 );

	FocalPlaneXResolutionDenChar[0]= ( unsigned char ) FocalPlaneXResolutionDen[0];
	FocalPlaneXResolutionDenChar[1]= ( unsigned char ) ( FocalPlaneXResolutionDen[0]>>8 );
	FocalPlaneXResolutionDenChar[2]= ( unsigned char ) ( FocalPlaneXResolutionDen[0]>>16 );
	FocalPlaneXResolutionDenChar[3]= ( unsigned char ) ( FocalPlaneXResolutionDen[0]>>24 );

	//WRITE THE EXPOSURE TIME NUMERATOR
	memcpy ( startoftiff+santemp, FocalPlaneXResolutionNumChar, 4 );

	memcpy ( startoftiff+santemp+4, FocalPlaneXResolutionDenChar, 4 );

	offset+=48;

	///////////////ENTRY NO 15 : FocalPlaneYResolution////////////////////////
	//write EXPOSURE TIME tag
	memcpy ( tempExif, FocalPlaneYResolutiontag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, FocalPlaneYResolutionformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFocalPlaneYResolution, 4 ); //sanjeev
	tempExif += 4 ;
	//write EXPOSURE TIME

	santemp= ( int ) ( offset );
	FocalPlaneYResolutionoffchar[0]= ( unsigned char ) santemp;
	FocalPlaneYResolutionoffchar[1]= ( unsigned char ) ( santemp>>8 );
	FocalPlaneYResolutionoffchar[2]= ( unsigned char ) ( santemp>>16 );
	FocalPlaneYResolutionoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the X-Resolution offset into the bitstream
	memcpy ( tempExif, FocalPlaneYResolutionoffchar, 4 );
	tempExif += 4 ;
	FocalPlaneYResolutionNum[0] = exifFileInfo->FocalPlaneYResolutionNum;
	FocalPlaneYResolutionDen[0] = exifFileInfo->FocalPlaneYResolutionDen;

	FocalPlaneYResolutionNumChar[0]= ( unsigned char ) FocalPlaneYResolutionNum[0];
	FocalPlaneYResolutionNumChar[1]= ( unsigned char ) ( FocalPlaneYResolutionNum[0]>>8 );
	FocalPlaneYResolutionNumChar[2]= ( unsigned char ) ( FocalPlaneYResolutionNum[0]>>16 );
	FocalPlaneYResolutionNumChar[3]= ( unsigned char ) ( FocalPlaneYResolutionNum[0]>>24 );

	FocalPlaneYResolutionDenChar[0]= ( unsigned char ) FocalPlaneYResolutionDen[0];
	FocalPlaneYResolutionDenChar[1]= ( unsigned char ) ( FocalPlaneYResolutionDen[0]>>8 );
	FocalPlaneYResolutionDenChar[2]= ( unsigned char ) ( FocalPlaneYResolutionDen[0]>>16 );
	FocalPlaneYResolutionDenChar[3]= ( unsigned char ) ( FocalPlaneYResolutionDen[0]>>24 );

	//WRITE THE EXPOSURE TIME NUMERATOR
	memcpy ( startoftiff+santemp, FocalPlaneYResolutionNumChar, 4 );

	memcpy ( startoftiff+santemp+4, FocalPlaneYResolutionDenChar, 4 );

	offset+=48;

	///////////////ENTRY NO 16 :FocalPlaneResolutionUnit////////////////////////
	//write ExposureProgram tag
	memcpy ( tempExif, FocalPlaneResolutionUnittag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, FocalPlaneResolutionUnitformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentFocalPlaneResolutionUnit, 4 );
	tempExif += 4 ;
	//write FocalPlaneResolutionUnit
	FocalPlaneResolutionUnit[0] = exifFileInfo->FocalPlaneResolutionUnit;
	FocalPlaneResolutionUnitChar[0] = ( unsigned char ) ( FocalPlaneResolutionUnit[0] );
	FocalPlaneResolutionUnitChar[1] = ( unsigned char ) ( FocalPlaneResolutionUnit[0]>>8 );
	FocalPlaneResolutionUnitChar[2] = ( unsigned char ) ( FocalPlaneResolutionUnit[0]>>16 );
	FocalPlaneResolutionUnitChar[3] = ( unsigned char ) ( FocalPlaneResolutionUnit[0]>>24 );

	memcpy ( tempExif, FocalPlaneResolutionUnitChar, 4 );
	tempExif += 4 ;
	///////////////ENTRY NO 17 :UserComments////////////////////////
	//write model tag
	memcpy ( tempExif, UserCommentstag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, UserCommentsformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentUserComments, 4 ); //sanjeev
	tempExif += 4 ;
	//write model
	//  strcpy(model,tpJEInfo->Model);
	UCHAR charsetId[8] = {0x41,0x53,0x43,0x49,0x49,0x00,0x00,0x00};
	memcpy(UserComments, charsetId, 8);
	memcpy ( UserComments+8,exifFileInfo->UserComments,142 );
	santemp= ( int ) ( offset );
	UserCommentsoffchar[0]= ( unsigned char ) santemp;
	UserCommentsoffchar[1]= ( unsigned char ) ( santemp>>8 );
	UserCommentsoffchar[2]= ( unsigned char ) ( santemp>>16 );
	UserCommentsoffchar[3]= ( unsigned char ) ( santemp>>24 );
	//write the User Comments offset into the bitstream
	memcpy ( tempExif, UserCommentsoffchar, 4 );
	tempExif += 4 ;
	memcpy ( startoftiff+santemp, UserComments, 150 );
	offset+=150;
	///////////////ENTRY NO 18 :WHITE BALANCE////////////////////////
	//write WhiteBalance tag
	memcpy ( tempExif, WhiteBalancetag, 2 );
	tempExif += 2 ;
	//write format
	memcpy ( tempExif, WhiteBalanceformat, 2 );
	tempExif += 2 ;
	//write no of component
	memcpy ( tempExif, NcomponentWhiteBalance, 4 );
	tempExif += 4 ;
	//write orientation mode
	WhiteBalance[0] = exifFileInfo->WhiteBalance;
	WhiteBalanceChar[0] = ( unsigned char ) ( WhiteBalance[0] );
	WhiteBalanceChar[1] = ( unsigned char ) ( WhiteBalance[0]>>8 );
	WhiteBalanceChar[2] = ( unsigned char ) ( WhiteBalance[0]>>16 );
	WhiteBalanceChar[3] = ( unsigned char ) ( WhiteBalance[0]>>24 );

	memcpy ( tempExif, WhiteBalanceChar, 4 );
	tempExif += 4 ;
	//////////////WRITE END OF ENTRY FLAG//////////////////
	memcpy ( tempExif, EndOfEntry, 4 );
	tempExif += 4 ;
	santemp= ( unsigned int ) ( offset );

	//////////////////////ENTRIES ARE OVER/////////////////////////////////
	ExifSize = ( santemp ) +8;
	ExifLen[1] = ( unsigned char ) ( ExifSize );
	ExifLen[0] = ( unsigned char ) ( ExifSize>>8 );

	if ( ExifSize > EXIF_FILE_SIZE + MAX_FILE_THUMB_SIZE - 2 || ExifSize < 0 )
	{
	printf( "makeExifFile	Invalid Exif size\r\n" );
	tempExif = NULL;
	*totalLen = 0;
	return -1;
	}

	tempExif = ExifInitialCount;
	memcpy ( tempExif, APP1Marker, 2 );
	tempExif += 2 ;
	memcpy ( tempExif, ExifLen, 2 );
	*totalLen = ExifSize + 2;
	printf ( "makeExifFile  totalLen : %d\n", *totalLen );
	return 0;

}

int easyexif::EXIFInfo::makeNewSatJpgFromBuf(const char* src_file, const char* dest_file, const char* exifBuf, int exifSize ){
	FILE* src_fp = fopen(src_file, "rb");
	if( NULL == src_fp ){
	    printf("++++++open src_file failed %s\n", src_file);
	    return -1;
    }

	fseek(src_fp, 0, SEEK_END);
	unsigned src_size = ftell(src_fp);
	printf("++++++get src_file size %lu\n", src_size);
	fseek(src_fp, 0, SEEK_SET);

	FILE* dest_fp = fopen(dest_file, "w+"); //qlh 请注意, 请注意,记得给读权限, 后面会进行文件读操作
	if(NULL == dest_fp){
		printf("++++++open dest_file failed %s\n", dest_file);
		fclose(src_fp);
		return -2;
	}

	char* buf_dest = (char*)malloc( src_size );
	if(buf_dest == NULL){
		printf("++++++malloc buf_dest failed\n");
		fclose(src_fp);
		fclose(dest_fp);
		return -3;
	}
	
	size_t readSize = fread(buf_dest, 1, src_size, src_fp);
	
	fclose(src_fp);
	if(readSize != src_size){
		printf("++++++read src_file  %s failed\n", src_file);
		fclose(dest_fp);
        free(buf_dest);
        return -4;
	}

	fseek(dest_fp, 0, SEEK_SET);
	size_t write_size = fwrite( buf_dest, 1, src_size, dest_fp );//copy src to dest jpg
	free(buf_dest);
	
	if( write_size != src_size ) {
		printf("++++++write dest_file %s failed\n", dest_file);
		fclose(dest_fp);
		return -5;
    }

	char *buf_tail = (char*)malloc( src_size );//src
	if(NULL == buf_tail){
		printf("++++++malloc buf_tail failed\n");
		fclose(dest_fp);
		return -6;
	}

	unsigned char buf_data[4];memset(buf_data, 0, 4);

	fseek(dest_fp, 0, SEEK_SET);
	fread(buf_data, 1, 4, dest_fp);

	if( buf_data[0] != 0xFF || buf_data[1] != 0xD8 ) {
	    printf("++++++error NOT 0xFF 0xDB++++buf_data 0:%#x, buf data 1:%#x\n", buf_data[0], buf_data[1]);
		fclose(dest_fp);
	    free(buf_tail);
	    return -7;
    }


	fseek(dest_fp, -2, SEEK_CUR);
	size_t _tail_size = fread(buf_tail, 1, src_size, dest_fp);
	if(_tail_size > 0){
		fseek(dest_fp, -_tail_size, SEEK_CUR);
		write_size = fwrite(exifBuf, 1, exifSize, dest_fp);
		if(write_size != exifSize){
			printf("++++++write exifBuf fail(dest_file)\n");
			free(buf_tail);
			free(dest_fp);
			return -8;
		}
		write_size = fwrite(buf_tail, 1, _tail_size, dest_fp);
		if(write_size != _tail_size){
			printf("++++++write _tail_size fail");
			free(buf_tail);
			free(dest_fp);
			return -9;
		}
		printf("After 0xFF 0xD8 has makeEif data: %d Byte, Src data: %d Byte, Total data: %d Byte\n",exifSize,_tail_size,exifSize+_tail_size);
	}

	free(buf_tail);
	fclose(dest_fp);
	return 0;
}

int easyexif::EXIFInfo::makeLocalSatJpgFromBuf(const char* src_file, const char* exifBuf, int exifSize){
	FILE* src_fp = fopen(src_file, "r+"); //需要读权限和写权限
	if( NULL == src_fp ){
	    printf("++++++open src_file failed %s\n", src_file);
	    return -1;
    }

	fseek(src_fp, 0, SEEK_END);
	unsigned src_size = ftell(src_fp);
	printf("++++++get src_file size %lu\n", src_size);
	fseek(src_fp, 0, SEEK_SET);

	char* buf_dest = (char*)malloc( src_size );
	if(buf_dest == NULL){
		printf("++++++malloc buf_dest failed\n");
		fclose(src_fp);
		return -2;
	}
	
	size_t readSize = fread(buf_dest, 1, src_size, src_fp);
	
	if(readSize != src_size){
		printf("++++++read src_file  %s failed\n", src_file);
		fclose(src_fp);
        free(buf_dest);
        return -3;
	}

	unsigned char buf_data[4];memset(buf_data, 0, 4);
	fseek(src_fp, 0, SEEK_SET);
	fread(buf_data, 1, 4, src_fp);

	if( buf_data[0] != 0xFF || buf_data[1] != 0xD8 ) {
	    printf("++++++error NOT 0xFF 0xDB++++buf_data 0:%#x, buf data 1:%#x\n", buf_data[0], buf_data[1]);
		fclose(src_fp);
	    free(buf_dest);
	    return -4;
    }

	fseek(src_fp, -2, SEEK_CUR);

	size_t write_size = fwrite(exifBuf, 1, exifSize, src_fp);
	if(write_size != exifSize){
		printf("++++++write exifBuf fail\n");
		fclose(src_fp);
		free(buf_dest);
		return -5;
	}

	write_size = fwrite(buf_dest+2, 1, src_size-2, src_fp);
	if(write_size != (src_size-2)){
		printf("++++++write buf_dest fail\n");
		fclose(src_fp);
		free(buf_dest);
		return -6;
	}

	printf("After 0xFF 0xD8 has makeEif data: %d Byte,Total data: %d Byte\n",exifSize, src_size+exifSize);
	fclose(src_fp);
	free(buf_dest);
	
	return 0;
}



void easyexif::EXIFInfo::clear() {
  // Strings
  ImageDescription = "";
  Make = "";
  Model = "";
  Software = "";
  DateTime = "";
  DateTimeOriginal = "";
  DateTimeDigitized = "";
  SubSecTimeOriginal = "";
  Copyright = "";

  // Shorts / unsigned / double
  ByteAlign = 0;
  Orientation = 0;

  BitsPerSample = 0;
  ExposureTime = 0;
  FNumber = 0;
  ExposureProgram = 0;
  ISOSpeedRatings = 0;
  ShutterSpeedValue = 0;
  ExposureBiasValue = 0;
  SubjectDistance = 0;
  FocalLength = 0;
  FocalLengthIn35mm = 0;
  Flash = 0;
  FlashReturnedLight = 0;
  FlashMode = 0;
  MeteringMode = 0;
  ImageWidth = 0;
  ImageHeight = 0;

  // Geolocation
  GeoLocation.Latitude = 0;
  GeoLocation.Longitude = 0;
  GeoLocation.Altitude = 0;
  GeoLocation.AltitudeRef = 0;
  GeoLocation.DOP = 0;
  GeoLocation.LatComponents.degrees = 0;
  GeoLocation.LatComponents.minutes = 0;
  GeoLocation.LatComponents.seconds = 0;
  GeoLocation.LatComponents.direction = '?';
  GeoLocation.LonComponents.degrees = 0;
  GeoLocation.LonComponents.minutes = 0;
  GeoLocation.LonComponents.seconds = 0;
  GeoLocation.LonComponents.direction = '?';

  // LensInfo
  LensInfo.FocalLengthMax = 0;
  LensInfo.FocalLengthMin = 0;
  LensInfo.FStopMax = 0;
  LensInfo.FStopMin = 0;
  LensInfo.FocalPlaneYResolution = 0;
  LensInfo.FocalPlaneXResolution = 0;
  LensInfo.FocalPlaneResolutionUnit = 0;
  LensInfo.Make = "";
  LensInfo.Model = "";
}
