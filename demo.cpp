#include <stdio.h>
#include "exif.h"

#include <string.h>


#define EXIF_FILE_SIZE 28800


int main(int argc, char *argv[]) {
	if (argc < 2) {
		printf("Usage: demo <Path to JPEG file>\n");
		return -1;
	}

	easyexif::EXIFInfo result;
	easyexif::ExifFileInfo *ExifInfo;

	//************************jpeg文件写入Exif头信息**********************************

	/*
	*1.初始化 easyexif::ExifFileInfo 结构体, 传入对应信息
	*2.调用 makeExifFile 生成jpeg exif信息块
	*3.调用 makeLocalSatJpgFromBuf 写入 jpeg 文件
	*/
	ExifInfo = ( easyexif::ExifFileInfo* ) malloc(sizeof(easyexif::ExifFileInfo));
	memset(ExifInfo, 0x00, sizeof(easyexif::ExifFileInfo));   
	//makeExifParam(ExifInfo);
	strcpy(ExifInfo->Make,"BSJ" );;
	strcpy(ExifInfo->Model,"BSJ" );
	strcpy(ExifInfo->CopyRight,"BSJ SoftWare Center" );
	strcpy(ExifInfo->Version, "***");
	strcpy(ExifInfo->UserComments,"version 1.1.4a.2021010614");
	printf("\nMake:%s  Modle:%s  CopyRight:%s  UserComments:%s \n", ExifInfo->Make,ExifInfo->Model,ExifInfo->CopyRight,ExifInfo->UserComments);

	char        *ExifBuf;
	unsigned   	ExifLen;
	unsigned    bufSize;
	bufSize = EXIF_FILE_SIZE;
	ExifBuf = ( char* ) malloc ( bufSize );
	memset ( ExifBuf, 0x00, bufSize );
	
	result.makeExifFile ( ExifBuf, &ExifLen, ExifInfo);

	//result.makeNewSatJpgFromBuf( argv[1], argv[2],ExifBuf, ExifLen);
	result.makeLocalSatJpgFromBuf(argv[1], ExifBuf, ExifLen);

	free(ExifInfo);
	free(ExifBuf);



	//************************获取jpeg文件 exif信息 example**********************************

	// Read the JPEG file into a buffer
	FILE *fp = fopen(argv[1], "rb");
	if (!fp) {
		printf("Can't open file.\n");
		return -1;
	}
	
	fseek(fp, 0, SEEK_END);
	unsigned long fsize = ftell(fp);
	rewind(fp);
	
	unsigned char *buf = new unsigned char[fsize];
	if (fread(buf, 1, fsize, fp) != fsize) {
		printf("Can't read file.\n");
		fclose(fp);
		delete[] buf;
		return -2;
	}
	fclose(fp);

	// Parse EXIF
	int code = result.parseFrom(buf, fsize);
	delete[] buf;
	if (code) {
		printf("Error parsing EXIF: code %d\n", code);
		return -3;
	}
	
	// Dump EXIF information
	printf("UserComment: %s\n", result.UserComment.c_str());
	printf("Camera make          : %s\n", result.Make.c_str());
	printf("Camera model         : %s\n", result.Model.c_str());
	printf("Software             : %s\n", result.Software.c_str());
	printf("Bits per sample      : %d\n", result.BitsPerSample);
	printf("Image width          : %d\n", result.ImageWidth);
	printf("Image height         : %d\n", result.ImageHeight);
	printf("Image description    : %s\n", result.ImageDescription.c_str());
	printf("Image orientation    : %d\n", result.Orientation);
	printf("Image copyright      : %s\n", result.Copyright.c_str());
	printf("Image date/time      : %s\n", result.DateTime.c_str());
	printf("Original date/time   : %s\n", result.DateTimeOriginal.c_str());
	printf("Digitize date/time   : %s\n", result.DateTimeDigitized.c_str());
	printf("Subsecond time       : %s\n", result.SubSecTimeOriginal.c_str());
	printf("Exposure time        : 1/%d s\n",(unsigned)(1.0 / result.ExposureTime));
	printf("F-stop               : f/%.1f\n", result.FNumber);
	printf("Exposure program     : %d\n", result.ExposureProgram);
	printf("ISO speed            : %d\n", result.ISOSpeedRatings);
	printf("Subject distance     : %f m\n", result.SubjectDistance);
	printf("Exposure bias        : %f EV\n", result.ExposureBiasValue);
	printf("Flash used?          : %d\n", result.Flash);
	printf("Flash returned light : %d\n", result.FlashReturnedLight);
	printf("Flash mode           : %d\n", result.FlashMode);
	printf("Metering mode        : %d\n", result.MeteringMode);
	printf("Lens focal length    : %f mm\n", result.FocalLength);
	printf("35mm focal length    : %u mm\n", result.FocalLengthIn35mm);
	printf("GPS Latitude         : %f deg (%f deg, %f min, %f sec %c)\n",
	result.GeoLocation.Latitude, result.GeoLocation.LatComponents.degrees,
	result.GeoLocation.LatComponents.minutes,
	result.GeoLocation.LatComponents.seconds,
	result.GeoLocation.LatComponents.direction);
	printf("GPS Longitude        : %f deg (%f deg, %f min, %f sec %c)\n",
	result.GeoLocation.Longitude, result.GeoLocation.LonComponents.degrees,
	result.GeoLocation.LonComponents.minutes,
	result.GeoLocation.LonComponents.seconds,
	result.GeoLocation.LonComponents.direction);
	printf("GPS Altitude         : %f m\n", result.GeoLocation.Altitude);
	printf("GPS Precision (DOP)  : %f\n", result.GeoLocation.DOP);
	printf("Lens min focal length: %f mm\n", result.LensInfo.FocalLengthMin);
	printf("Lens max focal length: %f mm\n", result.LensInfo.FocalLengthMax);
	printf("Lens f-stop min      : f/%.1f\n", result.LensInfo.FStopMin);
	printf("Lens f-stop max      : f/%.1f\n", result.LensInfo.FStopMax);
	printf("Lens make            : %s\n", result.LensInfo.Make.c_str());
	printf("Lens model           : %s\n", result.LensInfo.Model.c_str());
	printf("Focal plane XRes     : %f\n", result.LensInfo.FocalPlaneXResolution);
	printf("Focal plane YRes     : %f\n", result.LensInfo.FocalPlaneYResolution);

	return 0;
}
