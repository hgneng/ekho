"""
/***************************************************************************
 * Copyright (C) 2008-2024 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                          *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/
"""

from models.prompt_tts_modified.jets import JETSGenerator
from models.prompt_tts_modified.simbert import StyleEncoder
from transformers import AutoTokenizer
import os, sys, warnings, torch, glob, argparse
import numpy as np
from models.hifigan.get_vocoder import MAX_WAV_VALUE
import soundfile as sf
from yacs import config as CONFIG
from tqdm import tqdm

tokenizer = None
style_encoder = None
speaker2id = None
token2id = None
generator = None

def get_style_embedding(prompt, tokenizer, style_encoder):
    prompt = tokenizer([prompt], return_tensors="pt")
    input_ids = prompt["input_ids"]
    token_type_ids = prompt["token_type_ids"]
    attention_mask = prompt["attention_mask"]

    with torch.no_grad():
        output = style_encoder(
            input_ids=input_ids,
            token_type_ids=token_type_ids,
            attention_mask=attention_mask,
        )

    style_embedding = output["pooled_output"].cpu().squeeze().numpy()
    return style_embedding

def init(args, config):
    global tokenizer
    global style_encoder
    global speaker2id
    global token2id
    global generator

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    root_path = os.path.join(config.output_directory, args.logdir)
    ckpt_path = os.path.join(root_path,  "ckpt")
    files = os.listdir(ckpt_path)
    
    for file in files:
        if args.checkpoint:
            if file != args.checkpoint:
                continue

        checkpoint_path = os.path.join(ckpt_path, file)

        with open(config.model_config_path, 'r') as fin:
            conf = CONFIG.load_cfg(fin)
     
        conf.n_vocab = config.n_symbols
        conf.n_speaker = config.speaker_n_labels

        style_encoder = StyleEncoder(config)
        model_CKPT = torch.load(config.style_encoder_ckpt, map_location="cpu")
        model_ckpt = {}

        for key, value in model_CKPT['model'].items():
            new_key = key[7:]
            model_ckpt[new_key] = value

        style_encoder.load_state_dict(model_ckpt)
        generator = JETSGenerator(conf).to(device)
        model_CKPT = torch.load(checkpoint_path, map_location=device)
        generator.load_state_dict(model_CKPT['generator'])
        generator.eval()

        with open(config.token_list_path, 'r') as f:
            token2id = {t.strip():idx for idx, t, in enumerate(f.readlines())}

        with open(config.speaker2id_path, encoding='utf-8') as f:
            speaker2id = {t.strip():idx for idx, t in enumerate(f.readlines())}

        tokenizer = AutoTokenizer.from_pretrained(config.bert_path)

        if os.path.exists(root_path + "/test_audio/audio/" +f"{file}/"):
            r = glob.glob(root_path + "/test_audio/audio/" +f"{file}/*")
            for j in r:
                os.remove(j)

def inference(content, phonemes, filepath):
    global tokenizer
    global style_encoder
    global speaker2id
    global token2id
    global generator

    prompt = 'Happy'
    style_embedding = get_style_embedding(prompt, tokenizer, style_encoder)
    content_embedding = get_style_embedding(content, tokenizer, style_encoder)
    speaker = speaker2id['1093']
    text_int = [token2id[ph] for ph in phonemes.split()]
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    sequence = torch.from_numpy(np.array(text_int)).to(device).long().unsqueeze(0)
    sequence_len = torch.from_numpy(np.array([len(text_int)])).to(device)
    style_embedding = torch.from_numpy(style_embedding).to(device).unsqueeze(0)
    content_embedding = torch.from_numpy(content_embedding).to(device).unsqueeze(0)
    speaker = torch.from_numpy(np.array([speaker])).to(device)

    with torch.no_grad():
        infer_output = generator(
                inputs_ling=sequence,
                inputs_style_embedding=style_embedding,
                input_lengths=sequence_len,
                inputs_content_embedding=content_embedding,
                inputs_speaker=speaker,
                alpha=1.0
            )
        audio = infer_output["wav_predictions"].squeeze() * MAX_WAV_VALUE
        audio = audio.cpu().numpy().astype('int16')
        sf.write(filepath, data=audio, samplerate=config.sampling_rate)

def getPhonemes(content):
    return frontend_cn.g2p_cn(content)

if __name__ == '__main__':
    print("run!")
    #p = argparse.ArgumentParser()
    #p.add_argument('-d', '--logdir', type=str, required=True)
    #p.add_argument("-c", "--config_folder", type=str, required=True)
    #p.add_argument("--checkpoint", type=str, required=False, default='', help='inference specific checkpoint, e.g --checkpoint checkpoint_230000')
    #p.add_argument('-t', '--test_file', type=str, required=True, help='the absolute path of test file that is going to inference')

    #args = p.parse_args() 
    args = argparse.Namespace()
    args.logdir = 'prompt_tts_open_source_joint'
    args.config_folder = 'config/joint'
    args.checkpoint = 'g_00140000'
    #args.test_file = 'data/text.tts.txt'

    ##################################################
    #sys.path.append(os.path.dirname(os.path.abspath("__file__")) + "/" + args.config_folder)

    from config.joint.config import Config
    config = Config()
    ##################################################
    init(args, config)

    import socket
    import time 
    import frontend_cn
    # 创建一个TCP/IP套接字
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # 绑定服务器地址和端口号
    server_address = ('localhost', 20491)
    sock.bind(server_address)

    # 监听端口，最大连接数为1
    sock.listen(1)

    print(getPhonemes('123'))

    while True:
        print('等待连接...')
        connection, client_address = sock.accept()
        try:
            print('连接来自:', client_address)
            #while True:
            data = connection.recv(1024)
            if data:
                try:
                    content = data.decode()
                    print('收到数据:', content)
                    beginTime = time.time()
                    phonemes = getPhonemes(content)
                    filepath = "/tmp/emotivoiceOutput.wav"
                    inference(content, phonemes, filepath)
                    endTime = time.time()
                    print('耗时：', endTime - beginTime)
                    connection.sendall(filepath.encode())  # 发送数据给客户端
                except UnicodeDecodeError as e:
                    print("Caught UnicodeDecodeError:", e)
        finally:
            # 清理连接
            connection.close()

