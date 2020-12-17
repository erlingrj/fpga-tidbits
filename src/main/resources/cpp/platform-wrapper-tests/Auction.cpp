#include <iostream>
using namespace std;
#include <string.h>
#include "Auction.hpp"
#include "platform.h"

bool Run_Auction(WrapperRegDriver * platform) {
    Auction t(platform);

    cout << "Auction test" <<endl;
    cout << "Signature: " << hex << t.get_signature() << dec << endl;

    unsigned int reward_matrix[16] = {1,2,3,4,4,1,2,3,3,4,1,2,2,3,4,1};
    unsigned int bufsize = 16 * sizeof(unsigned int);

    void * accelBuf  = platform->allocAccelBuffer(bufsize);
    platform->copyBufferHostToAccel(reward_matrix, accelBuf, bufsize);

    t.set_baseAddr((AccelDblReg) accelBuf);
    t.set_byteCount(bufsize);

    t.set_start(1);

    while(t.get_finished() != 1);

    platform->deallocAccelBuffer(accelBuf);

    t.set_start(0);
    cout <<"Auction finished" <<endl;

    return true;
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_Auction(platform);

  deinitPlatform(platform);

  return 0;
}