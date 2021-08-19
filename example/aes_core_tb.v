`define RED 3
`timescale 1ns/10ps
module aes_tb;

// ========================================================================
// Signal declarations
// ===================
    reg           fiq_gclk; // The global clock must bear this name
    reg           rst_n;
    reg           m_init;
    reg           m_next;
    // fiq userdata (this pragma indicates the following signal can be
    // controlled by the adversary)
    reg [127:0]   datain;
    // fiq secretdata (this pragma indicates the following signal is the
    // secret - target of the attack)
    reg [255:0]   key;
    reg           enc_notdec = 1'b1;
    reg [ 60*3 -1:0] random;
    // fiq resultdata (this pragma indicates the following signal stores
    // device output)
    wire [127:0]   dataout;
    wire           done;
    wire           ready;
    reg keylen = 1'b0;
// ========================================================================

// ========================================================================
// DUT instantiation
// The instance name must be dut
// =============================

aes_core dut (
	  .clk(fiq_gclk)
	, .reset_n(rst_n)
	, .encdec(enc_notdec)
	, .init(m_init)
	, .next(m_next)
//	, .keylen(1'b0)       // May tie inputs to constants - there is a broblem with literal constants
                          //                               when using VPI-model translated from lib
	, .keylen(keylen)     //                               So need to assign reg or net to formal part
	, .key(key)
	, .block(datain)
	, .result(dataout)
	, .ready(ready)
	, .result_valid(done) // May leave outputs unconnected
);

// ========================================================================

// ========================================================================
// Reset task
// The name must be fiq_reset
// =========================
    task fiq_reset;
    begin
		m_next = 1'b0;
        m_init = 1'b0;
        rst_n = 1'b0;
        // Time maintenance by @(posedge fiq_gclk) only. Can be enclosed in the
        // 'repeat' construct
        repeat(5) @(posedge fiq_gclk);
        rst_n = 1'b1;
        #1; // Delays will be ignored by the FIQ tools
        @(posedge fiq_gclk);
     end
     endtask
// ========================================================================


// ========================================================================
// Single step task
// The name must be fiq_singlestep
// The data controlled by the adversary must be an input to the task
// TBD: parallel execution for pipeline
// ====================================
    task fiq_singlestep (input [255:0] curr_key, input [127:0] curr_indata, output [127:0] curr_outdata);
    begin
        key = curr_key;
        enc_notdec = 1'b1;
        // Time maintenance by @(posedge fiq_gclk) only. Can be enclosed in the
        // 'repeat' construct
        repeat (2) @(posedge fiq_gclk);
        m_init = 1'b1;
        @(posedge fiq_gclk);
        m_init = 1'b0;
        wait(ready==1'b1); // The FIQ simulator toggles the clock until the
                           // condition is satisfied
        @(posedge fiq_gclk);
        datain = curr_indata; // datain is controlled by the simulator, so the
                              // right-hand side is ignored
        m_next = 1'b1;
        @(posedge fiq_gclk);
        m_next = 1'b0;
        wait(ready==1'b1);
        curr_outdata = dataout;
        @(posedge fiq_gclk);
    end
    endtask
// ========================================================================

// ========================================================================
// This code is ignored by the FIQ tools
// =====================================

    // fiq ignore begin
    
    // Generate the clock. Not needed for the FIQ simulator
    parameter HALFPERIOD = 5;
    initial fiq_gclk = 1'b0;
    always #HALFPERIOD fiq_gclk = ~fiq_gclk;

    reg [255:0] nist_key_arr [0:15];
    reg [127:0] nist_datain_arr [0:15];
    reg [127:0] nist_dataout_arr [0:15];
    reg [127:0] out_buffer;
    integer cnt=0;
        
    integer i;
    
    // Test the functions
    initial
    begin
        // Assume there is some real code for the array initialization. Unimportant for
        // our example. But for example we using this simple initialization.
        for (i=0; i<16; i=i+1) begin
			nist_key_arr[i] = 256'h2b7e151628aed2a6abf7158809cf4f3c00000000000000000000000000000000;
			nist_datain_arr[i] = 128'h6bc1bee22e409f96e93d7e117393172a;
			nist_dataout_arr[i] = 128'h3ad77bb40d7a3660a89ecaf32466ef97;
		end
		
		fiq_reset;
		
        repeat(16)
        begin
            fiq_singlestep(nist_key_arr[cnt], nist_datain_arr[cnt], out_buffer);
            if (out_buffer != nist_dataout_arr[cnt])
                    $display ("FAILURE\n");
            cnt = (cnt + 1) % 16;
        end
        $finish;
    end
    // fiq ignore end

// ========================================================================
endmodule
