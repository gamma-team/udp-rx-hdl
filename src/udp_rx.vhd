-- Copyright 2017 Patrick Gauvin
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY udp_rx IS
    GENERIC (
        -- Input and output bus width in bytes, must be a power of 2
        width : POSITIVE := 8
    );
    PORT (
        -- All ports are assumed to be synchronous with Clk
        Clk : IN STD_LOGIC;
        Rst : IN STD_LOGIC;
        -- Data input bus for data from the IP layer.
        -- Byte offsets (all integer types are big endian):
        -- 0: Protocol
        -- 1: Source IP address
        -- 5: Destination IP address
        -- 9: IP datagram's data section
        Data_in : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        -- Assertion indicates which Data_in bytes are valid.
        Data_in_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        -- Asserted when the first data is available on Data_in
        Data_in_start : IN STD_LOGIC;
        -- Asserted when the last valid data is available on Data_in.
        Data_in_end : IN STD_LOGIC;
        -- Indicate that there has been an error in the current data stream.
        -- Data_in will be ignored until the next Data_in_start assertion.
        Data_in_err : IN STD_LOGIC;

        -- UDP payload data output bus to the application layer.
        -- Byte offsets (all integer types are big endian):
        -- 0: Source IP address
        -- 4: Source port
        -- 6: Destination port
        -- 8: UDP data payload
        Data_out : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
        -- Assertion indicates which Data_out bytes are valid.
        Data_out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
        -- Asserted when the first data is available on Data_out
        Data_out_start : OUT STD_LOGIC;
        -- Asserted when the last data is available on Data_out.
        Data_out_end : OUT STD_LOGIC;
        -- Indicate that there has been an error in the current datagram.
        -- Data_out should be ignored until the next Data_out_start assertion.
        Data_out_err : OUT STD_LOGIC
    );
END ENTITY;

ARCHITECTURE normal OF udp_rx IS
    COMPONENT stream_packer IS
        GENERIC (
            width : POSITIVE := 4
        );
        PORT (
            Clk : IN STD_LOGIC;
            Rstn : IN STD_LOGIC;
            In_data : IN STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            In_valid : IN STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            In_last : IN STD_LOGIC;
            In_ready : OUT STD_LOGIC;
            Out_data : OUT STD_LOGIC_VECTOR(width * 8 - 1 DOWNTO 0);
            Out_valid : OUT STD_LOGIC_VECTOR(width - 1 DOWNTO 0);
            Out_last : OUT STD_LOGIC;
            Out_ready : IN STD_LOGIC
        );
    END COMPONENT;

    CONSTANT UDP_PROTO : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"11";
    CONSTANT DATA_IN_OFF_PROTO : INTEGER := 0;
    CONSTANT DATA_IN_OFF_ADDR_SRC : INTEGER := 1;
    CONSTANT DATA_IN_OFF_ADDR_DST : INTEGER := 5;
    CONSTANT DATA_IN_HDR_LEN : INTEGER := 9;
    CONSTANT DATA_IN_OFF_UDP_HDR_PORT_SRC : INTEGER := 9;
    CONSTANT DATA_IN_OFF_UDP_HDR_PORT_DST : INTEGER := 11;
    CONSTANT DATA_IN_OFF_UDP_HDR_LEN : INTEGER := 13;
    CONSTANT DATA_IN_OFF_UDP_HDR_CHK : INTEGER := 15;
    CONSTANT DATA_IN_OFF_UDP_PAYLOAD : INTEGER := 17;

    TYPE DATA_BUS IS ARRAY (width - 1 DOWNTO 0)
        OF STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL rstn : STD_LOGIC;

    SIGNAL p0_data_in : DATA_BUS;
    SIGNAL p0_data_in_valid
        : STD_LOGIC_VECTOR(Data_in_valid'length - 1 DOWNTO 0);
    SIGNAL p0_data_in_start : STD_LOGIC;
    SIGNAL p0_data_in_end : STD_LOGIC;
    SIGNAL p0_data_in_err : STD_LOGIC;
    SIGNAL p0_addr_src : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p0_addr_dst : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL p0_udp_port_src : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p0_udp_port_dst : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL p0_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p0_udp_chk : UNSIGNED(15 DOWNTO 0);
    SIGNAL p0_addr_src_valid : BOOLEAN;
    SIGNAL p0_addr_dst_valid : BOOLEAN;
    SIGNAL p0_udp_port_src_valid : BOOLEAN;
    SIGNAL p0_udp_port_dst_valid : BOOLEAN;
    SIGNAL p0_udp_len_valid : BOOLEAN;
    SIGNAL p0_udp_chk_valid : BOOLEAN;
    -- Number of bytes read on input stream, 17 bits so a full UDP datagram
    -- plus our extra information is countable.
    SIGNAL p0_len_read : UNSIGNED(16 DOWNTO 0);
    SIGNAL p0_started : BOOLEAN;
    SIGNAL p0_hdr_done : BOOLEAN;

    SIGNAL p1_data_in : DATA_BUS;
    SIGNAL p1_data_in_valid
        : STD_LOGIC_VECTOR(p0_data_in_valid'length - 1 DOWNTO 0);
    SIGNAL p1_data_in_start : STD_LOGIC;
    SIGNAL p1_data_in_end : STD_LOGIC;
    SIGNAL p1_data_in_err : STD_LOGIC;
    SIGNAL p1_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p1_udp_chk : UNSIGNED(15 DOWNTO 0);
    SIGNAL p1_len_read : UNSIGNED(p0_len_read'length - 1 DOWNTO 0);
    SIGNAL p1_chk_accum : UNSIGNED(31 DOWNTO 0);

    SIGNAL p2_data_in : DATA_BUS;
    SIGNAL p2_data_in_valid
        : STD_LOGIC_VECTOR(p1_data_in_valid'length - 1 DOWNTO 0);
    SIGNAL p2_data_in_start : STD_LOGIC;
    SIGNAL p2_data_in_end : STD_LOGIC;
    SIGNAL p2_data_in_err : STD_LOGIC;
    SIGNAL p2_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2_udp_chk : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2_len_read : UNSIGNED(p1_len_read'length - 1 DOWNTO 0);
    SIGNAL p2_chk_accum : UNSIGNED(31 DOWNTO 0);
    SIGNAL p2_chk_addend : UNSIGNED(15 DOWNTO 0);
    SIGNAL p2_internal_off : UNSIGNED(15 DOWNTO 0);

    SIGNAL p3_data_in : DATA_BUS;
    SIGNAL p3_data_in_valid
        : STD_LOGIC_VECTOR(p2_data_in_valid'length - 1 DOWNTO 0);
    SIGNAL p3_data_in_start : STD_LOGIC;
    SIGNAL p3_data_in_end : STD_LOGIC;
    SIGNAL p3_data_in_err : STD_LOGIC;
    SIGNAL p3_udp_len : UNSIGNED(15 DOWNTO 0);
    SIGNAL p3_udp_chk : UNSIGNED(15 DOWNTO 0);
    SIGNAL p3_len_read : UNSIGNED(p2_len_read'length - 1 DOWNTO 0);
    SIGNAL p3_chk_final : UNSIGNED(15 DOWNTO 0);

    SIGNAL p4_data_in : DATA_BUS;
    SIGNAL p4_data_in_valid
        : STD_LOGIC_VECTOR(p3_data_in_valid'length - 1 DOWNTO 0);
    SIGNAL p4_data_in_start : STD_LOGIC;
    SIGNAL p4_data_in_end : STD_LOGIC;
    SIGNAL p4_data_in_err : STD_LOGIC;

    SIGNAL data_in_reg : DATA_BUS;
    SIGNAL data_in_valid_reg : STD_LOGIC_VECTOR(Data_in_valid'range);
    SIGNAL data_in_start_reg : STD_LOGIC;
    SIGNAL data_in_end_reg : STD_LOGIC;
    SIGNAL data_in_err_reg : STD_LOGIC;

    SIGNAL packed_data_in : DATA_BUS;
    SIGNAL packed_data_in_sig
        : STD_LOGIC_VECTOR(packed_data_in'length * 8 - 1 DOWNTO 0);
    SIGNAL packed_data_in_valid : STD_LOGIC_VECTOR(Data_in_valid'range);
    SIGNAL packed_data_in_start : STD_LOGIC;
    SIGNAL packed_data_in_end : STD_LOGIC;
    SIGNAL packed_data_in_err : STD_LOGIC;
BEGIN
    rstn <= NOT Rst;

    gen_in_data: FOR i IN 0 TO width - 1 GENERATE
        packed_data_in(i) <= packed_data_in_sig((i + 1) * 8 - 1 DOWNTO i * 8);
    END GENERATE;
    c_input_stream_packer: stream_packer
        GENERIC MAP (
            width => Data_in'length / 8
        )
        PORT MAP (
            Clk => Clk,
            Rstn => rstn,
            In_data => Data_in,
            In_valid => Data_in_valid,
            In_last => Data_in_end,
            In_ready => OPEN,
            Out_data => packed_data_in_sig,
            Out_valid => packed_data_in_valid,
            Out_last => packed_data_in_end,
            Out_ready => '1'
        );

    PROCESS(Clk)
        FUNCTION n_valid(v : STD_LOGIC_VECTOR)
            RETURN INTEGER IS
            VARIABLE count : INTEGER;
        BEGIN
            count := 0;
            FOR i IN v'range LOOP
                IF v(i) = '1' THEN
                    count := count + 1;
                END IF;
            END LOOP;
            RETURN count;
        END FUNCTION;

        VARIABLE p0_len_read_var : UNSIGNED(p0_len_read'length - 1 DOWNTO 0);
        VARIABLE p1_chk_accum_var
            : UNSIGNED(p1_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p2_chk_accum_var
            : UNSIGNED(p2_chk_accum'length - 1 DOWNTO 0);
        VARIABLE p2_chk_addend_var
            : UNSIGNED(p2_chk_addend'length - 1 DOWNTO 0);
        VARIABLE p2_internal_off_var
            : UNSIGNED(p2_internal_off'length - 1 DOWNTO 0);
    BEGIN
        IF rising_edge(Clk) THEN
            IF Rst = '1' THEN
                data_in_reg <= (OTHERS => (OTHERS => '0'));
                data_in_valid_reg <= (OTHERS => '0');
                data_in_start_reg <= '0';
                data_in_end_reg <= '0';
                data_in_err_reg <= '0';
                p0_data_in <= (OTHERS => x"00");
                p0_data_in_valid <= (OTHERS => '0');
                p0_data_in_start <= '0';
                p0_data_in_end <= '0';
                p0_data_in_err <= '0';
                p0_addr_src <= (OTHERS => '0');
                p0_addr_dst <= (OTHERS => '0');
                p0_udp_port_src <= (OTHERS => '0');
                p0_udp_port_dst <= (OTHERS => '0');
                p0_udp_len <= (OTHERS => '0');
                p0_udp_chk <= (OTHERS => '0');
                p0_addr_src_valid <= false;
                p0_addr_dst_valid <= false;
                p0_udp_port_src_valid <= false;
                p0_udp_port_dst_valid <= false;
                p0_udp_len_valid <= false;
                p0_udp_chk_valid <= false;
                p0_len_read <= (OTHERS => '0');
                p0_started <= false;
                p0_hdr_done <= false;

                p1_data_in <= (OTHERS => x"00");
                p1_data_in_valid <= (OTHERS => '0');
                p1_data_in_start <= '0';
                p1_data_in_end <= '0';
                p1_data_in_err <= '0';
                p1_udp_len <= (OTHERS => '0');
                p1_udp_chk <= (OTHERS => '0');
                p1_len_read <= (OTHERS => '0');
                p1_chk_accum <= (OTHERS => '0');

                p2_data_in <= (OTHERS => x"00");
                p2_data_in_valid <= (OTHERS => '0');
                p2_data_in_start <= '0';
                p2_data_in_end <= '0';
                p2_data_in_err <= '0';
                p2_udp_len <= (OTHERS => '0');
                p2_udp_chk <= (OTHERS => '0');
                p2_len_read <= (OTHERS => '0');
                p2_chk_accum <= (OTHERS => '0');
                p2_chk_addend <= (OTHERS => '0');
                p2_internal_off <= (OTHERS => '0');

                p3_data_in <= (OTHERS => x"00");
                p3_data_in_valid <= (OTHERS => '0');
                p3_data_in_start <= '0';
                p3_data_in_end <= '0';
                p3_data_in_err <= '0';
                p3_udp_len <= (OTHERS => '0');
                p3_len_read <= (OTHERS => '0');
                p3_udp_chk <= (OTHERS => '0');
                p3_chk_final <= (OTHERS => '0');

                p4_data_in <= (OTHERS => x"00");
                p4_data_in_valid <= (OTHERS => '0');
                p4_data_in_start <= '0';
                p4_data_in_end <= '0';
                p4_data_in_err <= '0';
            ELSE
                -- Input signal wiring
                FOR i IN data_in_reg'range LOOP
                    data_in_reg(i) <= Data_in((i + 1) * 8 - 1 DOWNTO i * 8);
                END LOOP;
                data_in_valid_reg <= Data_in_valid;
                data_in_start_reg <= Data_in_start;
                data_in_end_reg <= Data_in_end;
                data_in_err_reg <= Data_in_err;

                --
                -- Pipeline stage 0: Byte decoding
                --
                p0_data_in <= packed_data_in;
                p0_data_in_valid <= packed_data_in_valid;
                p0_data_in_end <= packed_data_in_end;
                p0_data_in_err <= '0'; -- FIXME --packed_data_in_err;

                p0_len_read_var := p0_len_read;
                p0_data_in_start <= '0';
                IF NOT p0_started AND packed_data_in_valid /= x"00" THEN
                    p0_started <= true;
                    p0_data_in_start <= '1';
                    p0_len_read_var := (OTHERS => '0');
                END IF;
                IF packed_data_in_end = '1' THEN
                    p0_started <= false;
                    p0_hdr_done <= false;
                END IF;

                p0_addr_src_valid <= false;
                p0_addr_dst_valid <= false;
                p0_udp_port_dst_valid <= false;
                p0_udp_port_src_valid <= false;
                p0_udp_len_valid <= false;
                p0_udp_chk_valid <= false;
                -- TODO: Make this generic, ran out of time
                IF packed_data_in_valid /= x"00" AND NOT p0_hdr_done THEN
                    IF NOT p0_started THEN
                        IF packed_data_in(0) /= UDP_PROTO THEN
                            p0_data_in_err <= '1';
                        END IF;
                        p0_addr_src(31 DOWNTO 24) <= packed_data_in(1);
                        p0_addr_src(23 DOWNTO 16) <= packed_data_in(2);
                        p0_addr_src(15 DOWNTO 8) <= packed_data_in(3);
                        p0_addr_src(7 DOWNTO 0) <= packed_data_in(4);
                        p0_addr_src_valid <= true;
                        p0_addr_dst(31 DOWNTO 24) <= packed_data_in(5);
                        p0_addr_dst(23 DOWNTO 16) <= packed_data_in(6);
                        p0_addr_dst(15 DOWNTO 8) <= packed_data_in(7);
                        p0_data_in_valid <= (OTHERS => '0');
                        -- Source address
                        p0_data_in_valid(4 DOWNTO 1) <= "1111";
                    ELSIF p0_addr_src_valid THEN
                        p0_addr_dst(7 DOWNTO 0) <= packed_data_in(0);
                        p0_addr_dst_valid <= true;
                        p0_udp_port_src(15 DOWNTO 8) <= packed_data_in(1);
                        p0_udp_port_src(7 DOWNTO 0) <= packed_data_in(2);
                        p0_udp_port_src_valid <= true;
                        p0_udp_port_dst(15 DOWNTO 8) <= packed_data_in(3);
                        p0_udp_port_dst(7 DOWNTO 0) <= packed_data_in(4);
                        p0_udp_port_dst_valid <= true;
                        p0_udp_len(15 DOWNTO 8)
                            <= UNSIGNED(packed_data_in(5));
                        p0_udp_len(7 DOWNTO 0)
                            <= UNSIGNED(packed_data_in(6));
                        p0_udp_len_valid <= true;
                        p0_udp_chk(15 DOWNTO 8)
                            <= UNSIGNED(packed_data_in(7));
                        p0_data_in_valid <= (OTHERS => '0');
                        -- Source and destination ports
                        p0_data_in_valid(4 DOWNTO 1) <= "1111";
                    ELSE
                        p0_udp_chk(7 DOWNTO 0)
                            <= UNSIGNED(packed_data_in(0));
                        p0_udp_chk_valid <= true;
                        p0_data_in_valid(0) <= '0';
                        p0_hdr_done <= true;
                    END IF;
                END IF;
                p0_len_read_var := p0_len_read_var
                    + TO_UNSIGNED(n_valid(packed_data_in_valid),
                    p0_len_read_var'length);
                p0_len_read <= p0_len_read_var;

                --
                -- Stage 1: UDP pseudo and normal header checksumming
                --
                p1_data_in <= p0_data_in;
                p1_data_in_valid <= p0_data_in_valid;
                p1_data_in_start <= p0_data_in_start;
                p1_data_in_end <= p0_data_in_end;
                p1_data_in_err <= p0_data_in_err;
                p1_udp_len <= p0_udp_len;
                p1_udp_chk <= p0_udp_chk;
                p1_len_read <= p0_len_read;

                p1_chk_accum_var := p1_chk_accum;
                IF p0_data_in_start = '1' THEN
                    p1_chk_accum_var := (OTHERS => '0');
                    p1_chk_accum_var(UDP_PROTO'length - 1 DOWNTO 0)
                        := UNSIGNED(UDP_PROTO);
                END IF;
                IF p0_addr_src_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_src(31 DOWNTO 16))
                        + UNSIGNED(p0_addr_src(15 DOWNTO 0));
                END IF;
                IF p0_addr_dst_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_addr_dst(31 DOWNTO 16))
                        + UNSIGNED(p0_addr_dst(15 DOWNTO 0));
                END IF;
                IF p0_udp_port_src_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_udp_port_src);
                END IF;
                IF p0_udp_port_dst_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var
                        + UNSIGNED(p0_udp_port_dst);
                END IF;
                -- Note: The length is included twice because of the pseudo
                -- header.
                IF p0_udp_len_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var + p0_udp_len
                        + p0_udp_len;
                END IF;
                IF p0_udp_chk_valid THEN
                    p1_chk_accum_var := p1_chk_accum_var + p0_udp_chk;
                END IF;
                p1_chk_accum <= p1_chk_accum_var;

                --
                -- Stage 2: Data payload checksumming
                --
                p2_data_in <= p1_data_in;
                p2_data_in_valid <= p1_data_in_valid;
                p2_data_in_start <= p1_data_in_start;
                p2_data_in_end <= p1_data_in_end;
                p2_data_in_err <= p1_data_in_err;
                p2_udp_len <= p1_udp_len;
                p2_udp_chk <= p1_udp_chk;
                p2_len_read <= p1_len_read;

                -- TODO: change internal offset variable to just a boolean
                p2_internal_off_var := p2_internal_off;
                p2_chk_addend_var := p2_chk_addend;
                p2_chk_accum_var := p1_chk_accum;
                IF p1_data_in_start = '1' THEN
                    p2_internal_off_var := (OTHERS => '0');
                    p2_chk_addend_var := (OTHERS => '0');
                    p2_chk_accum_var := (OTHERS => '0');
                END IF;
                -- Note: If this is too slow, split into stages that handle
                -- ranges of byte enables
                IF p1_len_read >= DATA_IN_OFF_UDP_PAYLOAD THEN
                    FOR i IN 0 TO width - 1 LOOP
                        IF p1_data_in_valid(i) = '1' THEN
                            IF 0 = p2_internal_off_var MOD 2 THEN
                                p2_chk_addend_var(15 DOWNTO 8)
                                    := UNSIGNED(p1_data_in(i));
                            ELSE
                                p2_chk_addend_var(7 DOWNTO 0)
                                    := UNSIGNED(p1_data_in(i));
                                p2_chk_accum_var := p2_chk_accum_var
                                    + p2_chk_addend_var;
                            END IF;
                            p2_internal_off_var := p2_internal_off_var + 1;
                        END IF;
                    END LOOP;
                    IF p1_data_in_end = '1' THEN
                        -- account for non-word-aligned data length
                        IF 1 = p2_internal_off_var MOD 2 THEN
                            p2_chk_addend_var(7 DOWNTO 0) := (OTHERS => '0');
                            p2_chk_accum_var := p2_chk_accum_var
                                + p2_chk_addend_var;
                        END IF;
                    END IF;
                END IF;
                p2_chk_accum <= p2_chk_accum_var;
                p2_chk_addend <= p2_chk_addend_var;
                p2_internal_off <= p2_internal_off_var;

                --
                -- Stage 3: Finish checksum
                --
                p3_data_in <= p2_data_in;
                p3_data_in_valid <= p2_data_in_valid;
                p3_data_in_start <= p2_data_in_start;
                p3_data_in_end <= p2_data_in_end;
                p3_data_in_err <= p2_data_in_err;
                p3_udp_len <= p2_udp_len;
                p3_udp_chk <= p2_udp_chk;
                p3_len_read <= p2_len_read;

                p3_chk_final <= p2_chk_accum(31 DOWNTO 16)
                    + p2_chk_accum(15 DOWNTO 0);

                --
                -- Stage 4: Validate checksum and perform length checks
                --
                p4_data_in <= p3_data_in;
                p4_data_in_valid <= p3_data_in_valid;
                p4_data_in_start <= p3_data_in_start;
                p4_data_in_end <= p3_data_in_end;
                p4_data_in_err <= p3_data_in_err;

                IF p3_len_read > DATA_IN_HDR_LEN THEN
                    -- Datagram read is larger than the header states
                    IF p3_udp_len < p3_len_read - DATA_IN_HDR_LEN THEN
                        p4_data_in_err <= '1';
                    END IF;
                END IF;
                IF p3_data_in_end = '1' THEN
                    IF p3_udp_chk /= 0 AND p3_chk_final /= 65535 THEN
                        p4_data_in_err <= '1';
                    END IF;
                    IF p3_udp_len /= p3_len_read - DATA_IN_HDR_LEN  THEN
                        p4_data_in_err <= '1';
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- Output signal wiring
    gen_out_data: FOR i IN 0 TO width - 1 GENERATE
        Data_out((i + 1) * 8 - 1 DOWNTO i * 8) <= p4_data_in(i);
    END GENERATE;
    Data_out_valid <= p4_data_in_valid;
    Data_out_start <= p4_data_in_start;
    Data_out_end <= p4_data_in_end;
    Data_out_err <= p4_data_in_err;
END ARCHITECTURE;
